{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.FailedItemDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.FailedItemDetails
  ( FailedItemDetails (..),

    -- * Smart constructor
    mkFailedItemDetails,

    -- * Lenses
    fidFailureCode,
    fidRetryable,
  )
where

import Network.AWS.Inspector.Types.FailedItemErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Includes details about the failed items.
--
-- /See:/ 'mkFailedItemDetails' smart constructor.
data FailedItemDetails = FailedItemDetails'
  { -- | The status code of a failed item.
    failureCode :: FailedItemErrorCode,
    -- | Indicates whether you can immediately retry a request for this item for a specified resource.
    retryable :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailedItemDetails' with the minimum fields required to make a request.
--
-- * 'failureCode' - The status code of a failed item.
-- * 'retryable' - Indicates whether you can immediately retry a request for this item for a specified resource.
mkFailedItemDetails ::
  -- | 'failureCode'
  FailedItemErrorCode ->
  -- | 'retryable'
  Lude.Bool ->
  FailedItemDetails
mkFailedItemDetails pFailureCode_ pRetryable_ =
  FailedItemDetails'
    { failureCode = pFailureCode_,
      retryable = pRetryable_
    }

-- | The status code of a failed item.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fidFailureCode :: Lens.Lens' FailedItemDetails FailedItemErrorCode
fidFailureCode = Lens.lens (failureCode :: FailedItemDetails -> FailedItemErrorCode) (\s a -> s {failureCode = a} :: FailedItemDetails)
{-# DEPRECATED fidFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | Indicates whether you can immediately retry a request for this item for a specified resource.
--
-- /Note:/ Consider using 'retryable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fidRetryable :: Lens.Lens' FailedItemDetails Lude.Bool
fidRetryable = Lens.lens (retryable :: FailedItemDetails -> Lude.Bool) (\s a -> s {retryable = a} :: FailedItemDetails)
{-# DEPRECATED fidRetryable "Use generic-lens or generic-optics with 'retryable' instead." #-}

instance Lude.FromJSON FailedItemDetails where
  parseJSON =
    Lude.withObject
      "FailedItemDetails"
      ( \x ->
          FailedItemDetails'
            Lude.<$> (x Lude..: "failureCode") Lude.<*> (x Lude..: "retryable")
      )
