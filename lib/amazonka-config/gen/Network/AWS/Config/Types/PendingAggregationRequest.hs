{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.PendingAggregationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.PendingAggregationRequest
  ( PendingAggregationRequest (..),

    -- * Smart constructor
    mkPendingAggregationRequest,

    -- * Lenses
    parRequesterAccountId,
    parRequesterAWSRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that represents the account ID and region of an aggregator account that is requesting authorization but is not yet authorized.
--
-- /See:/ 'mkPendingAggregationRequest' smart constructor.
data PendingAggregationRequest = PendingAggregationRequest'
  { requesterAccountId ::
      Lude.Maybe Lude.Text,
    requesterAWSRegion ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PendingAggregationRequest' with the minimum fields required to make a request.
--
-- * 'requesterAWSRegion' - The region requesting to aggregate data.
-- * 'requesterAccountId' - The 12-digit account ID of the account requesting to aggregate data.
mkPendingAggregationRequest ::
  PendingAggregationRequest
mkPendingAggregationRequest =
  PendingAggregationRequest'
    { requesterAccountId = Lude.Nothing,
      requesterAWSRegion = Lude.Nothing
    }

-- | The 12-digit account ID of the account requesting to aggregate data.
--
-- /Note:/ Consider using 'requesterAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parRequesterAccountId :: Lens.Lens' PendingAggregationRequest (Lude.Maybe Lude.Text)
parRequesterAccountId = Lens.lens (requesterAccountId :: PendingAggregationRequest -> Lude.Maybe Lude.Text) (\s a -> s {requesterAccountId = a} :: PendingAggregationRequest)
{-# DEPRECATED parRequesterAccountId "Use generic-lens or generic-optics with 'requesterAccountId' instead." #-}

-- | The region requesting to aggregate data.
--
-- /Note:/ Consider using 'requesterAWSRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parRequesterAWSRegion :: Lens.Lens' PendingAggregationRequest (Lude.Maybe Lude.Text)
parRequesterAWSRegion = Lens.lens (requesterAWSRegion :: PendingAggregationRequest -> Lude.Maybe Lude.Text) (\s a -> s {requesterAWSRegion = a} :: PendingAggregationRequest)
{-# DEPRECATED parRequesterAWSRegion "Use generic-lens or generic-optics with 'requesterAWSRegion' instead." #-}

instance Lude.FromJSON PendingAggregationRequest where
  parseJSON =
    Lude.withObject
      "PendingAggregationRequest"
      ( \x ->
          PendingAggregationRequest'
            Lude.<$> (x Lude..:? "RequesterAccountId")
            Lude.<*> (x Lude..:? "RequesterAwsRegion")
      )
