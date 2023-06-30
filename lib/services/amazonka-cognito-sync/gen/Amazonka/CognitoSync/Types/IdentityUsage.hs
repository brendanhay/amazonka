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
-- Module      : Amazonka.CognitoSync.Types.IdentityUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types.IdentityUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Usage information for the identity.
--
-- /See:/ 'newIdentityUsage' smart constructor.
data IdentityUsage = IdentityUsage'
  { -- | Total data storage for this identity.
    dataStorage :: Prelude.Maybe Prelude.Integer,
    -- | Number of datasets for the identity.
    datasetCount :: Prelude.Maybe Prelude.Int,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | Date on which the identity was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataStorage', 'identityUsage_dataStorage' - Total data storage for this identity.
--
-- 'datasetCount', 'identityUsage_datasetCount' - Number of datasets for the identity.
--
-- 'identityId', 'identityUsage_identityId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'identityPoolId', 'identityUsage_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'lastModifiedDate', 'identityUsage_lastModifiedDate' - Date on which the identity was last modified.
newIdentityUsage ::
  IdentityUsage
newIdentityUsage =
  IdentityUsage'
    { dataStorage = Prelude.Nothing,
      datasetCount = Prelude.Nothing,
      identityId = Prelude.Nothing,
      identityPoolId = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing
    }

-- | Total data storage for this identity.
identityUsage_dataStorage :: Lens.Lens' IdentityUsage (Prelude.Maybe Prelude.Integer)
identityUsage_dataStorage = Lens.lens (\IdentityUsage' {dataStorage} -> dataStorage) (\s@IdentityUsage' {} a -> s {dataStorage = a} :: IdentityUsage)

-- | Number of datasets for the identity.
identityUsage_datasetCount :: Lens.Lens' IdentityUsage (Prelude.Maybe Prelude.Int)
identityUsage_datasetCount = Lens.lens (\IdentityUsage' {datasetCount} -> datasetCount) (\s@IdentityUsage' {} a -> s {datasetCount = a} :: IdentityUsage)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
identityUsage_identityId :: Lens.Lens' IdentityUsage (Prelude.Maybe Prelude.Text)
identityUsage_identityId = Lens.lens (\IdentityUsage' {identityId} -> identityId) (\s@IdentityUsage' {} a -> s {identityId = a} :: IdentityUsage)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
identityUsage_identityPoolId :: Lens.Lens' IdentityUsage (Prelude.Maybe Prelude.Text)
identityUsage_identityPoolId = Lens.lens (\IdentityUsage' {identityPoolId} -> identityPoolId) (\s@IdentityUsage' {} a -> s {identityPoolId = a} :: IdentityUsage)

-- | Date on which the identity was last modified.
identityUsage_lastModifiedDate :: Lens.Lens' IdentityUsage (Prelude.Maybe Prelude.UTCTime)
identityUsage_lastModifiedDate = Lens.lens (\IdentityUsage' {lastModifiedDate} -> lastModifiedDate) (\s@IdentityUsage' {} a -> s {lastModifiedDate = a} :: IdentityUsage) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON IdentityUsage where
  parseJSON =
    Data.withObject
      "IdentityUsage"
      ( \x ->
          IdentityUsage'
            Prelude.<$> (x Data..:? "DataStorage")
            Prelude.<*> (x Data..:? "DatasetCount")
            Prelude.<*> (x Data..:? "IdentityId")
            Prelude.<*> (x Data..:? "IdentityPoolId")
            Prelude.<*> (x Data..:? "LastModifiedDate")
      )

instance Prelude.Hashable IdentityUsage where
  hashWithSalt _salt IdentityUsage' {..} =
    _salt
      `Prelude.hashWithSalt` dataStorage
      `Prelude.hashWithSalt` datasetCount
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` lastModifiedDate

instance Prelude.NFData IdentityUsage where
  rnf IdentityUsage' {..} =
    Prelude.rnf dataStorage
      `Prelude.seq` Prelude.rnf datasetCount
      `Prelude.seq` Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf lastModifiedDate
