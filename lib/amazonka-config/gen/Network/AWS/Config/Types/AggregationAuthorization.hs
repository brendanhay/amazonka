-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregationAuthorization
  ( AggregationAuthorization (..),

    -- * Smart constructor
    mkAggregationAuthorization,

    -- * Lenses
    aaCreationTime,
    aaAuthorizedAWSRegion,
    aaAggregationAuthorizationARN,
    aaAuthorizedAccountId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that represents the authorizations granted to aggregator accounts and regions.
--
-- /See:/ 'mkAggregationAuthorization' smart constructor.
data AggregationAuthorization = AggregationAuthorization'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    authorizedAWSRegion ::
      Lude.Maybe Lude.Text,
    aggregationAuthorizationARN ::
      Lude.Maybe Lude.Text,
    authorizedAccountId ::
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

-- | Creates a value of 'AggregationAuthorization' with the minimum fields required to make a request.
--
-- * 'aggregationAuthorizationARN' - The Amazon Resource Name (ARN) of the aggregation object.
-- * 'authorizedAWSRegion' - The region authorized to collect aggregated data.
-- * 'authorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
-- * 'creationTime' - The time stamp when the aggregation authorization was created.
mkAggregationAuthorization ::
  AggregationAuthorization
mkAggregationAuthorization =
  AggregationAuthorization'
    { creationTime = Lude.Nothing,
      authorizedAWSRegion = Lude.Nothing,
      aggregationAuthorizationARN = Lude.Nothing,
      authorizedAccountId = Lude.Nothing
    }

-- | The time stamp when the aggregation authorization was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaCreationTime :: Lens.Lens' AggregationAuthorization (Lude.Maybe Lude.Timestamp)
aaCreationTime = Lens.lens (creationTime :: AggregationAuthorization -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: AggregationAuthorization)
{-# DEPRECATED aaCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The region authorized to collect aggregated data.
--
-- /Note:/ Consider using 'authorizedAWSRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAuthorizedAWSRegion :: Lens.Lens' AggregationAuthorization (Lude.Maybe Lude.Text)
aaAuthorizedAWSRegion = Lens.lens (authorizedAWSRegion :: AggregationAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {authorizedAWSRegion = a} :: AggregationAuthorization)
{-# DEPRECATED aaAuthorizedAWSRegion "Use generic-lens or generic-optics with 'authorizedAWSRegion' instead." #-}

-- | The Amazon Resource Name (ARN) of the aggregation object.
--
-- /Note:/ Consider using 'aggregationAuthorizationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAggregationAuthorizationARN :: Lens.Lens' AggregationAuthorization (Lude.Maybe Lude.Text)
aaAggregationAuthorizationARN = Lens.lens (aggregationAuthorizationARN :: AggregationAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {aggregationAuthorizationARN = a} :: AggregationAuthorization)
{-# DEPRECATED aaAggregationAuthorizationARN "Use generic-lens or generic-optics with 'aggregationAuthorizationARN' instead." #-}

-- | The 12-digit account ID of the account authorized to aggregate data.
--
-- /Note:/ Consider using 'authorizedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAuthorizedAccountId :: Lens.Lens' AggregationAuthorization (Lude.Maybe Lude.Text)
aaAuthorizedAccountId = Lens.lens (authorizedAccountId :: AggregationAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {authorizedAccountId = a} :: AggregationAuthorization)
{-# DEPRECATED aaAuthorizedAccountId "Use generic-lens or generic-optics with 'authorizedAccountId' instead." #-}

instance Lude.FromJSON AggregationAuthorization where
  parseJSON =
    Lude.withObject
      "AggregationAuthorization"
      ( \x ->
          AggregationAuthorization'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "AuthorizedAwsRegion")
            Lude.<*> (x Lude..:? "AggregationAuthorizationArn")
            Lude.<*> (x Lude..:? "AuthorizedAccountId")
      )
