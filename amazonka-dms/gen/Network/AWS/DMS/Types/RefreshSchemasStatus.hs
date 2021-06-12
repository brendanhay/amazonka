{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.RefreshSchemasStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.RefreshSchemasStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
import qualified Network.AWS.Lens as Lens

-- | Provides information that describes status of a schema at an endpoint
-- specified by the @DescribeRefreshSchemaStatus@ operation.
--
-- /See:/ 'newRefreshSchemasStatus' smart constructor.
data RefreshSchemasStatus = RefreshSchemasStatus'
  { -- | The status of the schema.
    status :: Core.Maybe RefreshSchemasStatusTypeValue,
    -- | The last failure message for the schema.
    lastFailureMessage :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Core.Maybe Core.Text,
    -- | The date the schema was last refreshed.
    lastRefreshDate :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RefreshSchemasStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'refreshSchemasStatus_status' - The status of the schema.
--
-- 'lastFailureMessage', 'refreshSchemasStatus_lastFailureMessage' - The last failure message for the schema.
--
-- 'endpointArn', 'refreshSchemasStatus_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
--
-- 'lastRefreshDate', 'refreshSchemasStatus_lastRefreshDate' - The date the schema was last refreshed.
--
-- 'replicationInstanceArn', 'refreshSchemasStatus_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
newRefreshSchemasStatus ::
  RefreshSchemasStatus
newRefreshSchemasStatus =
  RefreshSchemasStatus'
    { status = Core.Nothing,
      lastFailureMessage = Core.Nothing,
      endpointArn = Core.Nothing,
      lastRefreshDate = Core.Nothing,
      replicationInstanceArn = Core.Nothing
    }

-- | The status of the schema.
refreshSchemasStatus_status :: Lens.Lens' RefreshSchemasStatus (Core.Maybe RefreshSchemasStatusTypeValue)
refreshSchemasStatus_status = Lens.lens (\RefreshSchemasStatus' {status} -> status) (\s@RefreshSchemasStatus' {} a -> s {status = a} :: RefreshSchemasStatus)

-- | The last failure message for the schema.
refreshSchemasStatus_lastFailureMessage :: Lens.Lens' RefreshSchemasStatus (Core.Maybe Core.Text)
refreshSchemasStatus_lastFailureMessage = Lens.lens (\RefreshSchemasStatus' {lastFailureMessage} -> lastFailureMessage) (\s@RefreshSchemasStatus' {} a -> s {lastFailureMessage = a} :: RefreshSchemasStatus)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
refreshSchemasStatus_endpointArn :: Lens.Lens' RefreshSchemasStatus (Core.Maybe Core.Text)
refreshSchemasStatus_endpointArn = Lens.lens (\RefreshSchemasStatus' {endpointArn} -> endpointArn) (\s@RefreshSchemasStatus' {} a -> s {endpointArn = a} :: RefreshSchemasStatus)

-- | The date the schema was last refreshed.
refreshSchemasStatus_lastRefreshDate :: Lens.Lens' RefreshSchemasStatus (Core.Maybe Core.UTCTime)
refreshSchemasStatus_lastRefreshDate = Lens.lens (\RefreshSchemasStatus' {lastRefreshDate} -> lastRefreshDate) (\s@RefreshSchemasStatus' {} a -> s {lastRefreshDate = a} :: RefreshSchemasStatus) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the replication instance.
refreshSchemasStatus_replicationInstanceArn :: Lens.Lens' RefreshSchemasStatus (Core.Maybe Core.Text)
refreshSchemasStatus_replicationInstanceArn = Lens.lens (\RefreshSchemasStatus' {replicationInstanceArn} -> replicationInstanceArn) (\s@RefreshSchemasStatus' {} a -> s {replicationInstanceArn = a} :: RefreshSchemasStatus)

instance Core.FromJSON RefreshSchemasStatus where
  parseJSON =
    Core.withObject
      "RefreshSchemasStatus"
      ( \x ->
          RefreshSchemasStatus'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "LastFailureMessage")
            Core.<*> (x Core..:? "EndpointArn")
            Core.<*> (x Core..:? "LastRefreshDate")
            Core.<*> (x Core..:? "ReplicationInstanceArn")
      )

instance Core.Hashable RefreshSchemasStatus

instance Core.NFData RefreshSchemasStatus
