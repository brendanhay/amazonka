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
-- Module      : Amazonka.CognitoSync.Types.IdentityPoolUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types.IdentityPoolUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Usage information for the identity pool.
--
-- /See:/ 'newIdentityPoolUsage' smart constructor.
data IdentityPoolUsage = IdentityPoolUsage'
  { -- | Data storage information for the identity pool.
    dataStorage :: Prelude.Maybe Prelude.Integer,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | Date on which the identity pool was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Number of sync sessions for the identity pool.
    syncSessionsCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityPoolUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataStorage', 'identityPoolUsage_dataStorage' - Data storage information for the identity pool.
--
-- 'identityPoolId', 'identityPoolUsage_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'lastModifiedDate', 'identityPoolUsage_lastModifiedDate' - Date on which the identity pool was last modified.
--
-- 'syncSessionsCount', 'identityPoolUsage_syncSessionsCount' - Number of sync sessions for the identity pool.
newIdentityPoolUsage ::
  IdentityPoolUsage
newIdentityPoolUsage =
  IdentityPoolUsage'
    { dataStorage = Prelude.Nothing,
      identityPoolId = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      syncSessionsCount = Prelude.Nothing
    }

-- | Data storage information for the identity pool.
identityPoolUsage_dataStorage :: Lens.Lens' IdentityPoolUsage (Prelude.Maybe Prelude.Integer)
identityPoolUsage_dataStorage = Lens.lens (\IdentityPoolUsage' {dataStorage} -> dataStorage) (\s@IdentityPoolUsage' {} a -> s {dataStorage = a} :: IdentityPoolUsage)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
identityPoolUsage_identityPoolId :: Lens.Lens' IdentityPoolUsage (Prelude.Maybe Prelude.Text)
identityPoolUsage_identityPoolId = Lens.lens (\IdentityPoolUsage' {identityPoolId} -> identityPoolId) (\s@IdentityPoolUsage' {} a -> s {identityPoolId = a} :: IdentityPoolUsage)

-- | Date on which the identity pool was last modified.
identityPoolUsage_lastModifiedDate :: Lens.Lens' IdentityPoolUsage (Prelude.Maybe Prelude.UTCTime)
identityPoolUsage_lastModifiedDate = Lens.lens (\IdentityPoolUsage' {lastModifiedDate} -> lastModifiedDate) (\s@IdentityPoolUsage' {} a -> s {lastModifiedDate = a} :: IdentityPoolUsage) Prelude.. Lens.mapping Data._Time

-- | Number of sync sessions for the identity pool.
identityPoolUsage_syncSessionsCount :: Lens.Lens' IdentityPoolUsage (Prelude.Maybe Prelude.Integer)
identityPoolUsage_syncSessionsCount = Lens.lens (\IdentityPoolUsage' {syncSessionsCount} -> syncSessionsCount) (\s@IdentityPoolUsage' {} a -> s {syncSessionsCount = a} :: IdentityPoolUsage)

instance Data.FromJSON IdentityPoolUsage where
  parseJSON =
    Data.withObject
      "IdentityPoolUsage"
      ( \x ->
          IdentityPoolUsage'
            Prelude.<$> (x Data..:? "DataStorage")
            Prelude.<*> (x Data..:? "IdentityPoolId")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "SyncSessionsCount")
      )

instance Prelude.Hashable IdentityPoolUsage where
  hashWithSalt _salt IdentityPoolUsage' {..} =
    _salt
      `Prelude.hashWithSalt` dataStorage
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` syncSessionsCount

instance Prelude.NFData IdentityPoolUsage where
  rnf IdentityPoolUsage' {..} =
    Prelude.rnf dataStorage
      `Prelude.seq` Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf syncSessionsCount
