{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.Parameter
  ( Parameter (..)
  -- * Smart constructor
  , mkParameter
  -- * Lenses
  , pAllowedValues
  , pApplyType
  , pDataType
  , pDescription
  , pIsModifiable
  , pMinimumEngineVersion
  , pParameterName
  , pParameterValue
  , pSource
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.ParameterApplyType as Types

-- | Describes a parameter in a cluster parameter group.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { allowedValues :: Core.Maybe Core.Text
    -- ^ The valid range of values for the parameter.
  , applyType :: Core.Maybe Types.ParameterApplyType
    -- ^ Specifies how to apply the WLM configuration parameter. Some properties can be applied dynamically, while other properties require that any associated clusters be rebooted for the configuration changes to be applied. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
  , dataType :: Core.Maybe Core.Text
    -- ^ The data type of the parameter.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the parameter.
  , isModifiable :: Core.Maybe Core.Bool
    -- ^ If @true@ , the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed. 
  , minimumEngineVersion :: Core.Maybe Core.Text
    -- ^ The earliest engine version to which the parameter can apply.
  , parameterName :: Core.Maybe Core.Text
    -- ^ The name of the parameter.
  , parameterValue :: Core.Maybe Core.Text
    -- ^ The value of the parameter.
  , source :: Core.Maybe Core.Text
    -- ^ The source of the parameter value, such as "engine-default" or "user".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Parameter' value with any optional fields omitted.
mkParameter
    :: Parameter
mkParameter
  = Parameter'{allowedValues = Core.Nothing,
               applyType = Core.Nothing, dataType = Core.Nothing,
               description = Core.Nothing, isModifiable = Core.Nothing,
               minimumEngineVersion = Core.Nothing, parameterName = Core.Nothing,
               parameterValue = Core.Nothing, source = Core.Nothing}

-- | The valid range of values for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowedValues :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pAllowedValues = Lens.field @"allowedValues"
{-# INLINEABLE pAllowedValues #-}
{-# DEPRECATED allowedValues "Use generic-lens or generic-optics with 'allowedValues' instead"  #-}

-- | Specifies how to apply the WLM configuration parameter. Some properties can be applied dynamically, while other properties require that any associated clusters be rebooted for the configuration changes to be applied. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pApplyType :: Lens.Lens' Parameter (Core.Maybe Types.ParameterApplyType)
pApplyType = Lens.field @"applyType"
{-# INLINEABLE pApplyType #-}
{-# DEPRECATED applyType "Use generic-lens or generic-optics with 'applyType' instead"  #-}

-- | The data type of the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDataType :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pDataType = Lens.field @"dataType"
{-# INLINEABLE pDataType #-}
{-# DEPRECATED dataType "Use generic-lens or generic-optics with 'dataType' instead"  #-}

-- | A description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pDescription = Lens.field @"description"
{-# INLINEABLE pDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | If @true@ , the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed. 
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIsModifiable :: Lens.Lens' Parameter (Core.Maybe Core.Bool)
pIsModifiable = Lens.field @"isModifiable"
{-# INLINEABLE pIsModifiable #-}
{-# DEPRECATED isModifiable "Use generic-lens or generic-optics with 'isModifiable' instead"  #-}

-- | The earliest engine version to which the parameter can apply.
--
-- /Note:/ Consider using 'minimumEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMinimumEngineVersion :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pMinimumEngineVersion = Lens.field @"minimumEngineVersion"
{-# INLINEABLE pMinimumEngineVersion #-}
{-# DEPRECATED minimumEngineVersion "Use generic-lens or generic-optics with 'minimumEngineVersion' instead"  #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterName :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pParameterName = Lens.field @"parameterName"
{-# INLINEABLE pParameterName #-}
{-# DEPRECATED parameterName "Use generic-lens or generic-optics with 'parameterName' instead"  #-}

-- | The value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterValue :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pParameterValue = Lens.field @"parameterValue"
{-# INLINEABLE pParameterValue #-}
{-# DEPRECATED parameterValue "Use generic-lens or generic-optics with 'parameterValue' instead"  #-}

-- | The source of the parameter value, such as "engine-default" or "user".
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSource :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pSource = Lens.field @"source"
{-# INLINEABLE pSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

instance Core.ToQuery Parameter where
        toQuery Parameter{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AllowedValues")
              allowedValues
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ApplyType") applyType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DataType") dataType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IsModifiable")
                isModifiable
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MinimumEngineVersion")
                minimumEngineVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ParameterName")
                parameterName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ParameterValue")
                parameterValue
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Source") source

instance Core.FromXML Parameter where
        parseXML x
          = Parameter' Core.<$>
              (x Core..@? "AllowedValues") Core.<*> x Core..@? "ApplyType"
                Core.<*> x Core..@? "DataType"
                Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "IsModifiable"
                Core.<*> x Core..@? "MinimumEngineVersion"
                Core.<*> x Core..@? "ParameterName"
                Core.<*> x Core..@? "ParameterValue"
                Core.<*> x Core..@? "Source"
