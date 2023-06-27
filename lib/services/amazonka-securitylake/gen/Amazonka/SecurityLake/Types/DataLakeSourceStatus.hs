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
-- Module      : Amazonka.SecurityLake.Types.DataLakeSourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeSourceStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.SourceCollectionStatus

-- | Retrieves the Logs status for the Amazon Security Lake account.
--
-- /See:/ 'newDataLakeSourceStatus' smart constructor.
data DataLakeSourceStatus = DataLakeSourceStatus'
  { -- | Defines path the stored logs are available which has information on your
    -- systems, applications, and services.
    resource :: Prelude.Maybe Prelude.Text,
    -- | The health status of services, including error codes and patterns.
    status :: Prelude.Maybe SourceCollectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeSourceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'dataLakeSourceStatus_resource' - Defines path the stored logs are available which has information on your
-- systems, applications, and services.
--
-- 'status', 'dataLakeSourceStatus_status' - The health status of services, including error codes and patterns.
newDataLakeSourceStatus ::
  DataLakeSourceStatus
newDataLakeSourceStatus =
  DataLakeSourceStatus'
    { resource = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Defines path the stored logs are available which has information on your
-- systems, applications, and services.
dataLakeSourceStatus_resource :: Lens.Lens' DataLakeSourceStatus (Prelude.Maybe Prelude.Text)
dataLakeSourceStatus_resource = Lens.lens (\DataLakeSourceStatus' {resource} -> resource) (\s@DataLakeSourceStatus' {} a -> s {resource = a} :: DataLakeSourceStatus)

-- | The health status of services, including error codes and patterns.
dataLakeSourceStatus_status :: Lens.Lens' DataLakeSourceStatus (Prelude.Maybe SourceCollectionStatus)
dataLakeSourceStatus_status = Lens.lens (\DataLakeSourceStatus' {status} -> status) (\s@DataLakeSourceStatus' {} a -> s {status = a} :: DataLakeSourceStatus)

instance Data.FromJSON DataLakeSourceStatus where
  parseJSON =
    Data.withObject
      "DataLakeSourceStatus"
      ( \x ->
          DataLakeSourceStatus'
            Prelude.<$> (x Data..:? "resource")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DataLakeSourceStatus where
  hashWithSalt _salt DataLakeSourceStatus' {..} =
    _salt
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` status

instance Prelude.NFData DataLakeSourceStatus where
  rnf DataLakeSourceStatus' {..} =
    Prelude.rnf resource
      `Prelude.seq` Prelude.rnf status
