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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RefreshSchemasStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.RefreshSchemasStatusTypeValue
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes status of a schema at an endpoint
-- specified by the @DescribeRefreshSchemaStatus@ operation.
--
-- /See:/ 'newRefreshSchemasStatus' smart constructor.
data RefreshSchemasStatus = RefreshSchemasStatus'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | The last failure message for the schema.
    lastFailureMessage :: Prelude.Maybe Prelude.Text,
    -- | The date the schema was last refreshed.
    lastRefreshDate :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the schema.
    status :: Prelude.Maybe RefreshSchemasStatusTypeValue
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
-- 'endpointArn', 'refreshSchemasStatus_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
--
-- 'lastFailureMessage', 'refreshSchemasStatus_lastFailureMessage' - The last failure message for the schema.
--
-- 'lastRefreshDate', 'refreshSchemasStatus_lastRefreshDate' - The date the schema was last refreshed.
--
-- 'replicationInstanceArn', 'refreshSchemasStatus_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
--
-- 'status', 'refreshSchemasStatus_status' - The status of the schema.
newRefreshSchemasStatus ::
  RefreshSchemasStatus
newRefreshSchemasStatus =
  RefreshSchemasStatus'
    { endpointArn =
        Prelude.Nothing,
      lastFailureMessage = Prelude.Nothing,
      lastRefreshDate = Prelude.Nothing,
      replicationInstanceArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
refreshSchemasStatus_endpointArn :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.Text)
refreshSchemasStatus_endpointArn = Lens.lens (\RefreshSchemasStatus' {endpointArn} -> endpointArn) (\s@RefreshSchemasStatus' {} a -> s {endpointArn = a} :: RefreshSchemasStatus)

-- | The last failure message for the schema.
refreshSchemasStatus_lastFailureMessage :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.Text)
refreshSchemasStatus_lastFailureMessage = Lens.lens (\RefreshSchemasStatus' {lastFailureMessage} -> lastFailureMessage) (\s@RefreshSchemasStatus' {} a -> s {lastFailureMessage = a} :: RefreshSchemasStatus)

-- | The date the schema was last refreshed.
refreshSchemasStatus_lastRefreshDate :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.UTCTime)
refreshSchemasStatus_lastRefreshDate = Lens.lens (\RefreshSchemasStatus' {lastRefreshDate} -> lastRefreshDate) (\s@RefreshSchemasStatus' {} a -> s {lastRefreshDate = a} :: RefreshSchemasStatus) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the replication instance.
refreshSchemasStatus_replicationInstanceArn :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.Text)
refreshSchemasStatus_replicationInstanceArn = Lens.lens (\RefreshSchemasStatus' {replicationInstanceArn} -> replicationInstanceArn) (\s@RefreshSchemasStatus' {} a -> s {replicationInstanceArn = a} :: RefreshSchemasStatus)

-- | The status of the schema.
refreshSchemasStatus_status :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe RefreshSchemasStatusTypeValue)
refreshSchemasStatus_status = Lens.lens (\RefreshSchemasStatus' {status} -> status) (\s@RefreshSchemasStatus' {} a -> s {status = a} :: RefreshSchemasStatus)

instance Data.FromJSON RefreshSchemasStatus where
  parseJSON =
    Data.withObject
      "RefreshSchemasStatus"
      ( \x ->
          RefreshSchemasStatus'
            Prelude.<$> (x Data..:? "EndpointArn")
            Prelude.<*> (x Data..:? "LastFailureMessage")
            Prelude.<*> (x Data..:? "LastRefreshDate")
            Prelude.<*> (x Data..:? "ReplicationInstanceArn")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable RefreshSchemasStatus where
  hashWithSalt _salt RefreshSchemasStatus' {..} =
    _salt
      `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` lastFailureMessage
      `Prelude.hashWithSalt` lastRefreshDate
      `Prelude.hashWithSalt` replicationInstanceArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData RefreshSchemasStatus where
  rnf RefreshSchemasStatus' {..} =
    Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf lastFailureMessage
      `Prelude.seq` Prelude.rnf lastRefreshDate
      `Prelude.seq` Prelude.rnf replicationInstanceArn
      `Prelude.seq` Prelude.rnf status
