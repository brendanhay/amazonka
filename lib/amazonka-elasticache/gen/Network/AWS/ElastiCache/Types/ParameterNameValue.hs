{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ParameterNameValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ParameterNameValue
  ( ParameterNameValue (..),

    -- * Smart constructor
    mkParameterNameValue,

    -- * Lenses
    pnvParameterName,
    pnvParameterValue,
  )
where

import qualified Network.AWS.ElastiCache.Types.ParameterName as Types
import qualified Network.AWS.ElastiCache.Types.ParameterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a name-value pair that is used to update the value of a parameter.
--
-- /See:/ 'mkParameterNameValue' smart constructor.
data ParameterNameValue = ParameterNameValue'
  { -- | The name of the parameter.
    parameterName :: Core.Maybe Types.ParameterName,
    -- | The value of the parameter.
    parameterValue :: Core.Maybe Types.ParameterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterNameValue' value with any optional fields omitted.
mkParameterNameValue ::
  ParameterNameValue
mkParameterNameValue =
  ParameterNameValue'
    { parameterName = Core.Nothing,
      parameterValue = Core.Nothing
    }

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvParameterName :: Lens.Lens' ParameterNameValue (Core.Maybe Types.ParameterName)
pnvParameterName = Lens.field @"parameterName"
{-# DEPRECATED pnvParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | The value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvParameterValue :: Lens.Lens' ParameterNameValue (Core.Maybe Types.ParameterValue)
pnvParameterValue = Lens.field @"parameterValue"
{-# DEPRECATED pnvParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}
