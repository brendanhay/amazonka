-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.RefreshSchemasStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.RefreshSchemasStatus
  ( RefreshSchemasStatus (..),

    -- * Smart constructor
    mkRefreshSchemasStatus,

    -- * Lenses
    rssStatus,
    rssLastRefreshDate,
    rssReplicationInstanceARN,
    rssEndpointARN,
    rssLastFailureMessage,
  )
where

import Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that describes status of a schema at an endpoint specified by the @DescribeRefreshSchemaStatus@ operation.
--
-- /See:/ 'mkRefreshSchemasStatus' smart constructor.
data RefreshSchemasStatus = RefreshSchemasStatus'
  { status ::
      Lude.Maybe RefreshSchemasStatusTypeValue,
    lastRefreshDate :: Lude.Maybe Lude.Timestamp,
    replicationInstanceARN :: Lude.Maybe Lude.Text,
    endpointARN :: Lude.Maybe Lude.Text,
    lastFailureMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RefreshSchemasStatus' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
-- * 'lastFailureMessage' - The last failure message for the schema.
-- * 'lastRefreshDate' - The date the schema was last refreshed.
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
-- * 'status' - The status of the schema.
mkRefreshSchemasStatus ::
  RefreshSchemasStatus
mkRefreshSchemasStatus =
  RefreshSchemasStatus'
    { status = Lude.Nothing,
      lastRefreshDate = Lude.Nothing,
      replicationInstanceARN = Lude.Nothing,
      endpointARN = Lude.Nothing,
      lastFailureMessage = Lude.Nothing
    }

-- | The status of the schema.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssStatus :: Lens.Lens' RefreshSchemasStatus (Lude.Maybe RefreshSchemasStatusTypeValue)
rssStatus = Lens.lens (status :: RefreshSchemasStatus -> Lude.Maybe RefreshSchemasStatusTypeValue) (\s a -> s {status = a} :: RefreshSchemasStatus)
{-# DEPRECATED rssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date the schema was last refreshed.
--
-- /Note:/ Consider using 'lastRefreshDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssLastRefreshDate :: Lens.Lens' RefreshSchemasStatus (Lude.Maybe Lude.Timestamp)
rssLastRefreshDate = Lens.lens (lastRefreshDate :: RefreshSchemasStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastRefreshDate = a} :: RefreshSchemasStatus)
{-# DEPRECATED rssLastRefreshDate "Use generic-lens or generic-optics with 'lastRefreshDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssReplicationInstanceARN :: Lens.Lens' RefreshSchemasStatus (Lude.Maybe Lude.Text)
rssReplicationInstanceARN = Lens.lens (replicationInstanceARN :: RefreshSchemasStatus -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceARN = a} :: RefreshSchemasStatus)
{-# DEPRECATED rssReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssEndpointARN :: Lens.Lens' RefreshSchemasStatus (Lude.Maybe Lude.Text)
rssEndpointARN = Lens.lens (endpointARN :: RefreshSchemasStatus -> Lude.Maybe Lude.Text) (\s a -> s {endpointARN = a} :: RefreshSchemasStatus)
{-# DEPRECATED rssEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | The last failure message for the schema.
--
-- /Note:/ Consider using 'lastFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssLastFailureMessage :: Lens.Lens' RefreshSchemasStatus (Lude.Maybe Lude.Text)
rssLastFailureMessage = Lens.lens (lastFailureMessage :: RefreshSchemasStatus -> Lude.Maybe Lude.Text) (\s a -> s {lastFailureMessage = a} :: RefreshSchemasStatus)
{-# DEPRECATED rssLastFailureMessage "Use generic-lens or generic-optics with 'lastFailureMessage' instead." #-}

instance Lude.FromJSON RefreshSchemasStatus where
  parseJSON =
    Lude.withObject
      "RefreshSchemasStatus"
      ( \x ->
          RefreshSchemasStatus'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LastRefreshDate")
            Lude.<*> (x Lude..:? "ReplicationInstanceArn")
            Lude.<*> (x Lude..:? "EndpointArn")
            Lude.<*> (x Lude..:? "LastFailureMessage")
      )
