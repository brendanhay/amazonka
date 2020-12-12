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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ShareError

-- | Information about the portfolio share operation.
--
-- /See:/ 'mkShareDetails' smart constructor.
data ShareDetails = ShareDetails'
  { shareErrors ::
      Lude.Maybe [ShareError],
    successfulShares :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShareDetails' with the minimum fields required to make a request.
--
-- * 'shareErrors' - List of errors.
-- * 'successfulShares' - List of accounts for whom the operation succeeded.
mkShareDetails ::
  ShareDetails
mkShareDetails =
  ShareDetails'
    { shareErrors = Lude.Nothing,
      successfulShares = Lude.Nothing
    }

-- | List of errors.
--
-- /Note:/ Consider using 'shareErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareErrors :: Lens.Lens' ShareDetails (Lude.Maybe [ShareError])
sdShareErrors = Lens.lens (shareErrors :: ShareDetails -> Lude.Maybe [ShareError]) (\s a -> s {shareErrors = a} :: ShareDetails)
{-# DEPRECATED sdShareErrors "Use generic-lens or generic-optics with 'shareErrors' instead." #-}

-- | List of accounts for whom the operation succeeded.
--
-- /Note:/ Consider using 'successfulShares' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSuccessfulShares :: Lens.Lens' ShareDetails (Lude.Maybe [Lude.Text])
sdSuccessfulShares = Lens.lens (successfulShares :: ShareDetails -> Lude.Maybe [Lude.Text]) (\s a -> s {successfulShares = a} :: ShareDetails)
{-# DEPRECATED sdSuccessfulShares "Use generic-lens or generic-optics with 'successfulShares' instead." #-}

instance Lude.FromJSON ShareDetails where
  parseJSON =
    Lude.withObject
      "ShareDetails"
      ( \x ->
          ShareDetails'
            Lude.<$> (x Lude..:? "ShareErrors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SuccessfulShares" Lude..!= Lude.mempty)
      )
