{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.AppValidationOutput
  ( AppValidationOutput (..)
  -- * Smart constructor
  , mkAppValidationOutput
  -- * Lenses
  , avoSsmOutput
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.SSMOutput as Types

-- | Output from validating an application.
--
-- /See:/ 'mkAppValidationOutput' smart constructor.
newtype AppValidationOutput = AppValidationOutput'
  { ssmOutput :: Core.Maybe Types.SSMOutput
    -- ^ Output from using SSM to validate the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AppValidationOutput' value with any optional fields omitted.
mkAppValidationOutput
    :: AppValidationOutput
mkAppValidationOutput
  = AppValidationOutput'{ssmOutput = Core.Nothing}

-- | Output from using SSM to validate the application.
--
-- /Note:/ Consider using 'ssmOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avoSsmOutput :: Lens.Lens' AppValidationOutput (Core.Maybe Types.SSMOutput)
avoSsmOutput = Lens.field @"ssmOutput"
{-# INLINEABLE avoSsmOutput #-}
{-# DEPRECATED ssmOutput "Use generic-lens or generic-optics with 'ssmOutput' instead"  #-}

instance Core.FromJSON AppValidationOutput where
        parseJSON
          = Core.withObject "AppValidationOutput" Core.$
              \ x -> AppValidationOutput' Core.<$> (x Core..:? "ssmOutput")
