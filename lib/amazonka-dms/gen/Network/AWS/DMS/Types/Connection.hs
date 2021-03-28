{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Connection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Connection
  ( Connection (..)
  -- * Smart constructor
  , mkConnection
  -- * Lenses
  , cEndpointArn
  , cEndpointIdentifier
  , cLastFailureMessage
  , cReplicationInstanceArn
  , cReplicationInstanceIdentifier
  , cStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of the connection between an endpoint and a replication instance, including Amazon Resource Names (ARNs) and the last error message issued.
--
-- /See:/ 'mkConnection' smart constructor.
data Connection = Connection'
  { endpointArn :: Core.Maybe Core.Text
    -- ^ The ARN string that uniquely identifies the endpoint.
  , endpointIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the endpoint. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
  , lastFailureMessage :: Core.Maybe Core.Text
    -- ^ The error message when the connection last failed.
  , replicationInstanceArn :: Core.Maybe Core.Text
    -- ^ The ARN of the replication instance.
  , replicationInstanceIdentifier :: Core.Maybe Core.Text
    -- ^ The replication instance identifier. This parameter is stored as a lowercase string.
  , status :: Core.Maybe Core.Text
    -- ^ The connection status. This parameter can return one of the following values:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Connection' value with any optional fields omitted.
mkConnection
    :: Connection
mkConnection
  = Connection'{endpointArn = Core.Nothing,
                endpointIdentifier = Core.Nothing,
                lastFailureMessage = Core.Nothing,
                replicationInstanceArn = Core.Nothing,
                replicationInstanceIdentifier = Core.Nothing,
                status = Core.Nothing}

-- | The ARN string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEndpointArn :: Lens.Lens' Connection (Core.Maybe Core.Text)
cEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE cEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The identifier of the endpoint. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'endpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEndpointIdentifier :: Lens.Lens' Connection (Core.Maybe Core.Text)
cEndpointIdentifier = Lens.field @"endpointIdentifier"
{-# INLINEABLE cEndpointIdentifier #-}
{-# DEPRECATED endpointIdentifier "Use generic-lens or generic-optics with 'endpointIdentifier' instead"  #-}

-- | The error message when the connection last failed.
--
-- /Note:/ Consider using 'lastFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastFailureMessage :: Lens.Lens' Connection (Core.Maybe Core.Text)
cLastFailureMessage = Lens.field @"lastFailureMessage"
{-# INLINEABLE cLastFailureMessage #-}
{-# DEPRECATED lastFailureMessage "Use generic-lens or generic-optics with 'lastFailureMessage' instead"  #-}

-- | The ARN of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReplicationInstanceArn :: Lens.Lens' Connection (Core.Maybe Core.Text)
cReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE cReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

-- | The replication instance identifier. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'replicationInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReplicationInstanceIdentifier :: Lens.Lens' Connection (Core.Maybe Core.Text)
cReplicationInstanceIdentifier = Lens.field @"replicationInstanceIdentifier"
{-# INLINEABLE cReplicationInstanceIdentifier #-}
{-# DEPRECATED replicationInstanceIdentifier "Use generic-lens or generic-optics with 'replicationInstanceIdentifier' instead"  #-}

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
cStatus :: Lens.Lens' Connection (Core.Maybe Core.Text)
cStatus = Lens.field @"status"
{-# INLINEABLE cStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON Connection where
        parseJSON
          = Core.withObject "Connection" Core.$
              \ x ->
                Connection' Core.<$>
                  (x Core..:? "EndpointArn") Core.<*> x Core..:? "EndpointIdentifier"
                    Core.<*> x Core..:? "LastFailureMessage"
                    Core.<*> x Core..:? "ReplicationInstanceArn"
                    Core.<*> x Core..:? "ReplicationInstanceIdentifier"
                    Core.<*> x Core..:? "Status"
