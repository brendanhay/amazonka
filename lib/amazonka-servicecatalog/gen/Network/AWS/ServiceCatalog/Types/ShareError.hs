{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ShareError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ShareError
  ( ShareError (..)
  -- * Smart constructor
  , mkShareError
  -- * Lenses
  , seAccounts
  , seError
  , seMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.AccountId as Types
import qualified Network.AWS.ServiceCatalog.Types.Error as Types
import qualified Network.AWS.ServiceCatalog.Types.Message as Types

-- | Errors that occurred during the portfolio share operation.
--
-- /See:/ 'mkShareError' smart constructor.
data ShareError = ShareError'
  { accounts :: Core.Maybe [Types.AccountId]
    -- ^ List of accounts impacted by the error.
  , error :: Core.Maybe Types.Error
    -- ^ Error type that happened when processing the operation.
  , message :: Core.Maybe Types.Message
    -- ^ Information about the error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ShareError' value with any optional fields omitted.
mkShareError
    :: ShareError
mkShareError
  = ShareError'{accounts = Core.Nothing, error = Core.Nothing,
                message = Core.Nothing}

-- | List of accounts impacted by the error.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seAccounts :: Lens.Lens' ShareError (Core.Maybe [Types.AccountId])
seAccounts = Lens.field @"accounts"
{-# INLINEABLE seAccounts #-}
{-# DEPRECATED accounts "Use generic-lens or generic-optics with 'accounts' instead"  #-}

-- | Error type that happened when processing the operation.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seError :: Lens.Lens' ShareError (Core.Maybe Types.Error)
seError = Lens.field @"error"
{-# INLINEABLE seError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

-- | Information about the error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMessage :: Lens.Lens' ShareError (Core.Maybe Types.Message)
seMessage = Lens.field @"message"
{-# INLINEABLE seMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON ShareError where
        parseJSON
          = Core.withObject "ShareError" Core.$
              \ x ->
                ShareError' Core.<$>
                  (x Core..:? "Accounts") Core.<*> x Core..:? "Error" Core.<*>
                    x Core..:? "Message"
