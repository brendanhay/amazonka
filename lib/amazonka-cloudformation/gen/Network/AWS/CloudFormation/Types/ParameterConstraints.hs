{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ParameterConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ParameterConstraints
  ( ParameterConstraints (..)
  -- * Smart constructor
  , mkParameterConstraints
  -- * Lenses
  , pcAllowedValues
  ) where

import qualified Network.AWS.CloudFormation.Types.AllowedValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A set of criteria that AWS CloudFormation uses to validate parameter values. Although other constraints might be defined in the stack template, AWS CloudFormation returns only the @AllowedValues@ property.
--
-- /See:/ 'mkParameterConstraints' smart constructor.
newtype ParameterConstraints = ParameterConstraints'
  { allowedValues :: Core.Maybe [Types.AllowedValue]
    -- ^ A list of values that are permitted for a parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterConstraints' value with any optional fields omitted.
mkParameterConstraints
    :: ParameterConstraints
mkParameterConstraints
  = ParameterConstraints'{allowedValues = Core.Nothing}

-- | A list of values that are permitted for a parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcAllowedValues :: Lens.Lens' ParameterConstraints (Core.Maybe [Types.AllowedValue])
pcAllowedValues = Lens.field @"allowedValues"
{-# INLINEABLE pcAllowedValues #-}
{-# DEPRECATED allowedValues "Use generic-lens or generic-optics with 'allowedValues' instead"  #-}

instance Core.FromXML ParameterConstraints where
        parseXML x
          = ParameterConstraints' Core.<$>
              (x Core..@? "AllowedValues" Core..<@> Core.parseXMLList "member")
