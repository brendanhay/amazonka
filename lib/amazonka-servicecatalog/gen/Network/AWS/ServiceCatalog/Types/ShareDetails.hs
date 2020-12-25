{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ShareDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ShareDetails
  ( ShareDetails (..),

    -- * Smart constructor
    mkShareDetails,

    -- * Lenses
    sdShareErrors,
    sdSuccessfulShares,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.AccountId as Types
import qualified Network.AWS.ServiceCatalog.Types.ShareError as Types

-- | Information about the portfolio share operation.
--
-- /See:/ 'mkShareDetails' smart constructor.
data ShareDetails = ShareDetails'
  { -- | List of errors.
    shareErrors :: Core.Maybe [Types.ShareError],
    -- | List of accounts for whom the operation succeeded.
    successfulShares :: Core.Maybe [Types.AccountId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ShareDetails' value with any optional fields omitted.
mkShareDetails ::
  ShareDetails
mkShareDetails =
  ShareDetails'
    { shareErrors = Core.Nothing,
      successfulShares = Core.Nothing
    }

-- | List of errors.
--
-- /Note:/ Consider using 'shareErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareErrors :: Lens.Lens' ShareDetails (Core.Maybe [Types.ShareError])
sdShareErrors = Lens.field @"shareErrors"
{-# DEPRECATED sdShareErrors "Use generic-lens or generic-optics with 'shareErrors' instead." #-}

-- | List of accounts for whom the operation succeeded.
--
-- /Note:/ Consider using 'successfulShares' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSuccessfulShares :: Lens.Lens' ShareDetails (Core.Maybe [Types.AccountId])
sdSuccessfulShares = Lens.field @"successfulShares"
{-# DEPRECATED sdSuccessfulShares "Use generic-lens or generic-optics with 'successfulShares' instead." #-}

instance Core.FromJSON ShareDetails where
  parseJSON =
    Core.withObject "ShareDetails" Core.$
      \x ->
        ShareDetails'
          Core.<$> (x Core..:? "ShareErrors") Core.<*> (x Core..:? "SuccessfulShares")
