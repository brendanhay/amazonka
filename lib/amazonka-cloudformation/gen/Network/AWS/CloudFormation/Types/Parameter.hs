{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Parameter
  ( Parameter (..)
  -- * Smart constructor
  , mkParameter
  -- * Lenses
  , pParameterKey
  , pParameterValue
  , pResolvedValue
  , pUsePreviousValue
  ) where

import qualified Network.AWS.CloudFormation.Types.ParameterKey as Types
import qualified Network.AWS.CloudFormation.Types.ParameterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Parameter data type.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { parameterKey :: Core.Maybe Types.ParameterKey
    -- ^ The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation uses the default value that is specified in your template.
  , parameterValue :: Core.Maybe Types.ParameterValue
    -- ^ The input value associated with the parameter.
  , resolvedValue :: Core.Maybe Types.ParameterValue
    -- ^ Read-only. The value that corresponds to a Systems Manager parameter key. This field is returned only for <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types @SSM@ parameter types> in the template.
  , usePreviousValue :: Core.Maybe Core.Bool
    -- ^ During a stack update, use the existing parameter value that the stack is using for a given parameter key. If you specify @true@ , do not specify a parameter value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Parameter' value with any optional fields omitted.
mkParameter
    :: Parameter
mkParameter
  = Parameter'{parameterKey = Core.Nothing,
               parameterValue = Core.Nothing, resolvedValue = Core.Nothing,
               usePreviousValue = Core.Nothing}

-- | The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation uses the default value that is specified in your template.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterKey :: Lens.Lens' Parameter (Core.Maybe Types.ParameterKey)
pParameterKey = Lens.field @"parameterKey"
{-# INLINEABLE pParameterKey #-}
{-# DEPRECATED parameterKey "Use generic-lens or generic-optics with 'parameterKey' instead"  #-}

-- | The input value associated with the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterValue :: Lens.Lens' Parameter (Core.Maybe Types.ParameterValue)
pParameterValue = Lens.field @"parameterValue"
{-# INLINEABLE pParameterValue #-}
{-# DEPRECATED parameterValue "Use generic-lens or generic-optics with 'parameterValue' instead"  #-}

-- | Read-only. The value that corresponds to a Systems Manager parameter key. This field is returned only for <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types @SSM@ parameter types> in the template.
--
-- /Note:/ Consider using 'resolvedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResolvedValue :: Lens.Lens' Parameter (Core.Maybe Types.ParameterValue)
pResolvedValue = Lens.field @"resolvedValue"
{-# INLINEABLE pResolvedValue #-}
{-# DEPRECATED resolvedValue "Use generic-lens or generic-optics with 'resolvedValue' instead"  #-}

-- | During a stack update, use the existing parameter value that the stack is using for a given parameter key. If you specify @true@ , do not specify a parameter value.
--
-- /Note:/ Consider using 'usePreviousValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pUsePreviousValue :: Lens.Lens' Parameter (Core.Maybe Core.Bool)
pUsePreviousValue = Lens.field @"usePreviousValue"
{-# INLINEABLE pUsePreviousValue #-}
{-# DEPRECATED usePreviousValue "Use generic-lens or generic-optics with 'usePreviousValue' instead"  #-}

instance Core.ToQuery Parameter where
        toQuery Parameter{..}
          = Core.maybe Core.mempty (Core.toQueryPair "ParameterKey")
              parameterKey
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ParameterValue")
                parameterValue
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResolvedValue")
                resolvedValue
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UsePreviousValue")
                usePreviousValue

instance Core.FromXML Parameter where
        parseXML x
          = Parameter' Core.<$>
              (x Core..@? "ParameterKey") Core.<*> x Core..@? "ParameterValue"
                Core.<*> x Core..@? "ResolvedValue"
                Core.<*> x Core..@? "UsePreviousValue"
