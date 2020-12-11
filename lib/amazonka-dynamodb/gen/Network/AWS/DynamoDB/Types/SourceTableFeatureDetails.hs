-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
  ( SourceTableFeatureDetails (..),

    -- * Smart constructor
    mkSourceTableFeatureDetails,

    -- * Lenses
    stfdStreamDescription,
    stfdGlobalSecondaryIndexes,
    stfdLocalSecondaryIndexes,
    stfdSSEDescription,
    stfdTimeToLiveDescription,
  )
where

import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo
import Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo
import Network.AWS.DynamoDB.Types.SSEDescription
import Network.AWS.DynamoDB.Types.StreamSpecification
import Network.AWS.DynamoDB.Types.TimeToLiveDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
--
-- /See:/ 'mkSourceTableFeatureDetails' smart constructor.
data SourceTableFeatureDetails = SourceTableFeatureDetails'
  { streamDescription ::
      Lude.Maybe StreamSpecification,
    globalSecondaryIndexes ::
      Lude.Maybe [GlobalSecondaryIndexInfo],
    localSecondaryIndexes ::
      Lude.Maybe [LocalSecondaryIndexInfo],
    sSEDescription ::
      Lude.Maybe SSEDescription,
    timeToLiveDescription ::
      Lude.Maybe TimeToLiveDescription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceTableFeatureDetails' with the minimum fields required to make a request.
--
-- * 'globalSecondaryIndexes' - Represents the GSI properties for the table when the backup was created. It includes the IndexName, KeySchema, Projection, and ProvisionedThroughput for the GSIs on the table at the time of backup.
-- * 'localSecondaryIndexes' - Represents the LSI properties for the table when the backup was created. It includes the IndexName, KeySchema and Projection for the LSIs on the table at the time of backup.
-- * 'sSEDescription' - The description of the server-side encryption status on the table when the backup was created.
-- * 'streamDescription' - Stream settings on the table when the backup was created.
-- * 'timeToLiveDescription' - Time to Live settings on the table when the backup was created.
mkSourceTableFeatureDetails ::
  SourceTableFeatureDetails
mkSourceTableFeatureDetails =
  SourceTableFeatureDetails'
    { streamDescription = Lude.Nothing,
      globalSecondaryIndexes = Lude.Nothing,
      localSecondaryIndexes = Lude.Nothing,
      sSEDescription = Lude.Nothing,
      timeToLiveDescription = Lude.Nothing
    }

-- | Stream settings on the table when the backup was created.
--
-- /Note:/ Consider using 'streamDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfdStreamDescription :: Lens.Lens' SourceTableFeatureDetails (Lude.Maybe StreamSpecification)
stfdStreamDescription = Lens.lens (streamDescription :: SourceTableFeatureDetails -> Lude.Maybe StreamSpecification) (\s a -> s {streamDescription = a} :: SourceTableFeatureDetails)
{-# DEPRECATED stfdStreamDescription "Use generic-lens or generic-optics with 'streamDescription' instead." #-}

-- | Represents the GSI properties for the table when the backup was created. It includes the IndexName, KeySchema, Projection, and ProvisionedThroughput for the GSIs on the table at the time of backup.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfdGlobalSecondaryIndexes :: Lens.Lens' SourceTableFeatureDetails (Lude.Maybe [GlobalSecondaryIndexInfo])
stfdGlobalSecondaryIndexes = Lens.lens (globalSecondaryIndexes :: SourceTableFeatureDetails -> Lude.Maybe [GlobalSecondaryIndexInfo]) (\s a -> s {globalSecondaryIndexes = a} :: SourceTableFeatureDetails)
{-# DEPRECATED stfdGlobalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead." #-}

-- | Represents the LSI properties for the table when the backup was created. It includes the IndexName, KeySchema and Projection for the LSIs on the table at the time of backup.
--
-- /Note:/ Consider using 'localSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfdLocalSecondaryIndexes :: Lens.Lens' SourceTableFeatureDetails (Lude.Maybe [LocalSecondaryIndexInfo])
stfdLocalSecondaryIndexes = Lens.lens (localSecondaryIndexes :: SourceTableFeatureDetails -> Lude.Maybe [LocalSecondaryIndexInfo]) (\s a -> s {localSecondaryIndexes = a} :: SourceTableFeatureDetails)
{-# DEPRECATED stfdLocalSecondaryIndexes "Use generic-lens or generic-optics with 'localSecondaryIndexes' instead." #-}

-- | The description of the server-side encryption status on the table when the backup was created.
--
-- /Note:/ Consider using 'sSEDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfdSSEDescription :: Lens.Lens' SourceTableFeatureDetails (Lude.Maybe SSEDescription)
stfdSSEDescription = Lens.lens (sSEDescription :: SourceTableFeatureDetails -> Lude.Maybe SSEDescription) (\s a -> s {sSEDescription = a} :: SourceTableFeatureDetails)
{-# DEPRECATED stfdSSEDescription "Use generic-lens or generic-optics with 'sSEDescription' instead." #-}

-- | Time to Live settings on the table when the backup was created.
--
-- /Note:/ Consider using 'timeToLiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfdTimeToLiveDescription :: Lens.Lens' SourceTableFeatureDetails (Lude.Maybe TimeToLiveDescription)
stfdTimeToLiveDescription = Lens.lens (timeToLiveDescription :: SourceTableFeatureDetails -> Lude.Maybe TimeToLiveDescription) (\s a -> s {timeToLiveDescription = a} :: SourceTableFeatureDetails)
{-# DEPRECATED stfdTimeToLiveDescription "Use generic-lens or generic-optics with 'timeToLiveDescription' instead." #-}

instance Lude.FromJSON SourceTableFeatureDetails where
  parseJSON =
    Lude.withObject
      "SourceTableFeatureDetails"
      ( \x ->
          SourceTableFeatureDetails'
            Lude.<$> (x Lude..:? "StreamDescription")
            Lude.<*> (x Lude..:? "GlobalSecondaryIndexes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LocalSecondaryIndexes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SSEDescription")
            Lude.<*> (x Lude..:? "TimeToLiveDescription")
      )
