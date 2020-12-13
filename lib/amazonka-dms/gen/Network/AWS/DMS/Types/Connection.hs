{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Connection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Connection
  ( Connection (..),

    -- * Smart constructor
    mkConnection,

    -- * Lenses
    cStatus,
    cReplicationInstanceARN,
    cEndpointIdentifier,
    cReplicationInstanceIdentifier,
    cEndpointARN,
    cLastFailureMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status of the connection between an endpoint and a replication instance, including Amazon Resource Names (ARNs) and the last error message issued.
--
-- /See:/ 'mkConnection' smart constructor.
data Connection = Connection'
  { -- | The connection status. This parameter can return one of the following values:
    --
    --
    --     * @"successful"@
    --
    --
    --     * @"testing"@
    --
    --
    --     * @"failed"@
    --
    --
    --     * @"deleting"@
    status :: Lude.Maybe Lude.Text,
    -- | The ARN of the replication instance.
    replicationInstanceARN :: Lude.Maybe Lude.Text,
    -- | The identifier of the endpoint. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
    endpointIdentifier :: Lude.Maybe Lude.Text,
    -- | The replication instance identifier. This parameter is stored as a lowercase string.
    replicationInstanceIdentifier :: Lude.Maybe Lude.Text,
    -- | The ARN string that uniquely identifies the endpoint.
    endpointARN :: Lude.Maybe Lude.Text,
    -- | The error message when the connection last failed.
    lastFailureMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- * 'status' - The connection status. This parameter can return one of the following values:
--
--
--     * @"successful"@
--
--
--     * @"testing"@
--
--
--     * @"failed"@
--
--
--     * @"deleting"@
--
--
-- * 'replicationInstanceARN' - The ARN of the replication instance.
-- * 'endpointIdentifier' - The identifier of the endpoint. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
-- * 'replicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a lowercase string.
-- * 'endpointARN' - The ARN string that uniquely identifies the endpoint.
-- * 'lastFailureMessage' - The error message when the connection last failed.
mkConnection ::
  Connection
mkConnection =
  Connection'
    { status = Lude.Nothing,
      replicationInstanceARN = Lude.Nothing,
      endpointIdentifier = Lude.Nothing,
      replicationInstanceIdentifier = Lude.Nothing,
      endpointARN = Lude.Nothing,
      lastFailureMessage = Lude.Nothing
    }

-- | The connection status. This parameter can return one of the following values:
--
--
--     * @"successful"@
--
--
--     * @"testing"@
--
--
--     * @"failed"@
--
--
--     * @"deleting"@
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cStatus = Lens.lens (status :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Connection)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReplicationInstanceARN :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cReplicationInstanceARN = Lens.lens (replicationInstanceARN :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceARN = a} :: Connection)
{-# DEPRECATED cReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

-- | The identifier of the endpoint. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'endpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEndpointIdentifier :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cEndpointIdentifier = Lens.lens (endpointIdentifier :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {endpointIdentifier = a} :: Connection)
{-# DEPRECATED cEndpointIdentifier "Use generic-lens or generic-optics with 'endpointIdentifier' instead." #-}

-- | The replication instance identifier. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'replicationInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReplicationInstanceIdentifier :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cReplicationInstanceIdentifier = Lens.lens (replicationInstanceIdentifier :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceIdentifier = a} :: Connection)
{-# DEPRECATED cReplicationInstanceIdentifier "Use generic-lens or generic-optics with 'replicationInstanceIdentifier' instead." #-}

-- | The ARN string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEndpointARN :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cEndpointARN = Lens.lens (endpointARN :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {endpointARN = a} :: Connection)
{-# DEPRECATED cEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | The error message when the connection last failed.
--
-- /Note:/ Consider using 'lastFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastFailureMessage :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cLastFailureMessage = Lens.lens (lastFailureMessage :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {lastFailureMessage = a} :: Connection)
{-# DEPRECATED cLastFailureMessage "Use generic-lens or generic-optics with 'lastFailureMessage' instead." #-}

instance Lude.FromJSON Connection where
  parseJSON =
    Lude.withObject
      "Connection"
      ( \x ->
          Connection'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ReplicationInstanceArn")
            Lude.<*> (x Lude..:? "EndpointIdentifier")
            Lude.<*> (x Lude..:? "ReplicationInstanceIdentifier")
            Lude.<*> (x Lude..:? "EndpointArn")
            Lude.<*> (x Lude..:? "LastFailureMessage")
      )
