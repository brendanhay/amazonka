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
-- Module      : Amazonka.ServiceCatalog.Types.SourceConnectionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.SourceConnectionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.LastSync
import Amazonka.ServiceCatalog.Types.SourceConnectionParameters
import Amazonka.ServiceCatalog.Types.SourceType

-- | Provides details about the configured @SourceConnection@.
--
-- /See:/ 'newSourceConnectionDetail' smart constructor.
data SourceConnectionDetail = SourceConnectionDetail'
  { -- | The connection details based on the connection @Type@.
    connectionParameters :: Prelude.Maybe SourceConnectionParameters,
    -- | Provides details about the product\'s connection sync and contains the
    -- following sub-fields.
    --
    -- -   @LastSyncTime@
    --
    -- -   @LastSyncStatus@
    --
    -- -   @LastSyncStatusMessage@
    --
    -- -   @LastSuccessfulSyncTime@
    --
    -- -   @LastSuccessfulSyncProvisioningArtifactID@
    lastSync :: Prelude.Maybe LastSync,
    -- | The only supported @SourceConnection@ type is Codestar.
    type' :: Prelude.Maybe SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceConnectionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionParameters', 'sourceConnectionDetail_connectionParameters' - The connection details based on the connection @Type@.
--
-- 'lastSync', 'sourceConnectionDetail_lastSync' - Provides details about the product\'s connection sync and contains the
-- following sub-fields.
--
-- -   @LastSyncTime@
--
-- -   @LastSyncStatus@
--
-- -   @LastSyncStatusMessage@
--
-- -   @LastSuccessfulSyncTime@
--
-- -   @LastSuccessfulSyncProvisioningArtifactID@
--
-- 'type'', 'sourceConnectionDetail_type' - The only supported @SourceConnection@ type is Codestar.
newSourceConnectionDetail ::
  SourceConnectionDetail
newSourceConnectionDetail =
  SourceConnectionDetail'
    { connectionParameters =
        Prelude.Nothing,
      lastSync = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The connection details based on the connection @Type@.
sourceConnectionDetail_connectionParameters :: Lens.Lens' SourceConnectionDetail (Prelude.Maybe SourceConnectionParameters)
sourceConnectionDetail_connectionParameters = Lens.lens (\SourceConnectionDetail' {connectionParameters} -> connectionParameters) (\s@SourceConnectionDetail' {} a -> s {connectionParameters = a} :: SourceConnectionDetail)

-- | Provides details about the product\'s connection sync and contains the
-- following sub-fields.
--
-- -   @LastSyncTime@
--
-- -   @LastSyncStatus@
--
-- -   @LastSyncStatusMessage@
--
-- -   @LastSuccessfulSyncTime@
--
-- -   @LastSuccessfulSyncProvisioningArtifactID@
sourceConnectionDetail_lastSync :: Lens.Lens' SourceConnectionDetail (Prelude.Maybe LastSync)
sourceConnectionDetail_lastSync = Lens.lens (\SourceConnectionDetail' {lastSync} -> lastSync) (\s@SourceConnectionDetail' {} a -> s {lastSync = a} :: SourceConnectionDetail)

-- | The only supported @SourceConnection@ type is Codestar.
sourceConnectionDetail_type :: Lens.Lens' SourceConnectionDetail (Prelude.Maybe SourceType)
sourceConnectionDetail_type = Lens.lens (\SourceConnectionDetail' {type'} -> type') (\s@SourceConnectionDetail' {} a -> s {type' = a} :: SourceConnectionDetail)

instance Data.FromJSON SourceConnectionDetail where
  parseJSON =
    Data.withObject
      "SourceConnectionDetail"
      ( \x ->
          SourceConnectionDetail'
            Prelude.<$> (x Data..:? "ConnectionParameters")
            Prelude.<*> (x Data..:? "LastSync")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable SourceConnectionDetail where
  hashWithSalt _salt SourceConnectionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` connectionParameters
      `Prelude.hashWithSalt` lastSync
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SourceConnectionDetail where
  rnf SourceConnectionDetail' {..} =
    Prelude.rnf connectionParameters `Prelude.seq`
      Prelude.rnf lastSync `Prelude.seq`
        Prelude.rnf type'
