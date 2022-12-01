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
-- Module      : Amazonka.DMS.Types.RefreshSchemasStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RefreshSchemasStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.RefreshSchemasStatusTypeValue
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes status of a schema at an endpoint
-- specified by the @DescribeRefreshSchemaStatus@ operation.
--
-- /See:/ 'newRefreshSchemasStatus' smart constructor.
data RefreshSchemasStatus = RefreshSchemasStatus'
  { -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The date the schema was last refreshed.
    lastRefreshDate :: Prelude.Maybe Core.POSIX,
    -- | The last failure message for the schema.
    lastFailureMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the schema.
    status :: Prelude.Maybe RefreshSchemasStatusTypeValue,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshSchemasStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationInstanceArn', 'refreshSchemasStatus_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
--
-- 'lastRefreshDate', 'refreshSchemasStatus_lastRefreshDate' - The date the schema was last refreshed.
--
-- 'lastFailureMessage', 'refreshSchemasStatus_lastFailureMessage' - The last failure message for the schema.
--
-- 'status', 'refreshSchemasStatus_status' - The status of the schema.
--
-- 'endpointArn', 'refreshSchemasStatus_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
newRefreshSchemasStatus ::
  RefreshSchemasStatus
newRefreshSchemasStatus =
  RefreshSchemasStatus'
    { replicationInstanceArn =
        Prelude.Nothing,
      lastRefreshDate = Prelude.Nothing,
      lastFailureMessage = Prelude.Nothing,
      status = Prelude.Nothing,
      endpointArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replication instance.
refreshSchemasStatus_replicationInstanceArn :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.Text)
refreshSchemasStatus_replicationInstanceArn = Lens.lens (\RefreshSchemasStatus' {replicationInstanceArn} -> replicationInstanceArn) (\s@RefreshSchemasStatus' {} a -> s {replicationInstanceArn = a} :: RefreshSchemasStatus)

-- | The date the schema was last refreshed.
refreshSchemasStatus_lastRefreshDate :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.UTCTime)
refreshSchemasStatus_lastRefreshDate = Lens.lens (\RefreshSchemasStatus' {lastRefreshDate} -> lastRefreshDate) (\s@RefreshSchemasStatus' {} a -> s {lastRefreshDate = a} :: RefreshSchemasStatus) Prelude.. Lens.mapping Core._Time

-- | The last failure message for the schema.
refreshSchemasStatus_lastFailureMessage :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.Text)
refreshSchemasStatus_lastFailureMessage = Lens.lens (\RefreshSchemasStatus' {lastFailureMessage} -> lastFailureMessage) (\s@RefreshSchemasStatus' {} a -> s {lastFailureMessage = a} :: RefreshSchemasStatus)

-- | The status of the schema.
refreshSchemasStatus_status :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe RefreshSchemasStatusTypeValue)
refreshSchemasStatus_status = Lens.lens (\RefreshSchemasStatus' {status} -> status) (\s@RefreshSchemasStatus' {} a -> s {status = a} :: RefreshSchemasStatus)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
refreshSchemasStatus_endpointArn :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.Text)
refreshSchemasStatus_endpointArn = Lens.lens (\RefreshSchemasStatus' {endpointArn} -> endpointArn) (\s@RefreshSchemasStatus' {} a -> s {endpointArn = a} :: RefreshSchemasStatus)

instance Core.FromJSON RefreshSchemasStatus where
  parseJSON =
    Core.withObject
      "RefreshSchemasStatus"
      ( \x ->
          RefreshSchemasStatus'
            Prelude.<$> (x Core..:? "ReplicationInstanceArn")
            Prelude.<*> (x Core..:? "LastRefreshDate")
            Prelude.<*> (x Core..:? "LastFailureMessage")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "EndpointArn")
      )

instance Prelude.Hashable RefreshSchemasStatus where
  hashWithSalt _salt RefreshSchemasStatus' {..} =
    _salt `Prelude.hashWithSalt` replicationInstanceArn
      `Prelude.hashWithSalt` lastRefreshDate
      `Prelude.hashWithSalt` lastFailureMessage
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData RefreshSchemasStatus where
  rnf RefreshSchemasStatus' {..} =
    Prelude.rnf replicationInstanceArn
      `Prelude.seq` Prelude.rnf lastRefreshDate
      `Prelude.seq` Prelude.rnf lastFailureMessage
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endpointArn
