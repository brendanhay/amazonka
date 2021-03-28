{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ValidationWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ValidationWarning
  ( ValidationWarning (..)
  -- * Smart constructor
  , mkValidationWarning
  -- * Lenses
  , vwErrors
  ) where

import qualified Network.AWS.EC2.Types.ValidationError as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The error codes and error messages that are returned for the parameters or parameter combinations that are not valid when a new launch template or new version of a launch template is created.
--
-- /See:/ 'mkValidationWarning' smart constructor.
newtype ValidationWarning = ValidationWarning'
  { errors :: Core.Maybe [Types.ValidationError]
    -- ^ The error codes and error messages.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ValidationWarning' value with any optional fields omitted.
mkValidationWarning
    :: ValidationWarning
mkValidationWarning = ValidationWarning'{errors = Core.Nothing}

-- | The error codes and error messages.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vwErrors :: Lens.Lens' ValidationWarning (Core.Maybe [Types.ValidationError])
vwErrors = Lens.field @"errors"
{-# INLINEABLE vwErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

instance Core.FromXML ValidationWarning where
        parseXML x
          = ValidationWarning' Core.<$>
              (x Core..@? "errorSet" Core..<@> Core.parseXMLList "item")
