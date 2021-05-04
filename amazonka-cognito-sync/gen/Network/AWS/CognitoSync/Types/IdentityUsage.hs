{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoSync.Types.IdentityUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.IdentityUsage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Usage information for the identity.
--
-- /See:/ 'newIdentityUsage' smart constructor.
data IdentityUsage = IdentityUsage'
  { -- | Date on which the identity was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | Number of datasets for the identity.
    datasetCount :: Prelude.Maybe Prelude.Int,
    -- | Total data storage for this identity.
    dataStorage :: Prelude.Maybe Prelude.Integer,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IdentityUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'identityUsage_lastModifiedDate' - Date on which the identity was last modified.
--
-- 'identityPoolId', 'identityUsage_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'datasetCount', 'identityUsage_datasetCount' - Number of datasets for the identity.
--
-- 'dataStorage', 'identityUsage_dataStorage' - Total data storage for this identity.
--
-- 'identityId', 'identityUsage_identityId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
newIdentityUsage ::
  IdentityUsage
newIdentityUsage =
  IdentityUsage'
    { lastModifiedDate = Prelude.Nothing,
      identityPoolId = Prelude.Nothing,
      datasetCount = Prelude.Nothing,
      dataStorage = Prelude.Nothing,
      identityId = Prelude.Nothing
    }

-- | Date on which the identity was last modified.
identityUsage_lastModifiedDate :: Lens.Lens' IdentityUsage (Prelude.Maybe Prelude.UTCTime)
identityUsage_lastModifiedDate = Lens.lens (\IdentityUsage' {lastModifiedDate} -> lastModifiedDate) (\s@IdentityUsage' {} a -> s {lastModifiedDate = a} :: IdentityUsage) Prelude.. Lens.mapping Prelude._Time

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
identityUsage_identityPoolId :: Lens.Lens' IdentityUsage (Prelude.Maybe Prelude.Text)
identityUsage_identityPoolId = Lens.lens (\IdentityUsage' {identityPoolId} -> identityPoolId) (\s@IdentityUsage' {} a -> s {identityPoolId = a} :: IdentityUsage)

-- | Number of datasets for the identity.
identityUsage_datasetCount :: Lens.Lens' IdentityUsage (Prelude.Maybe Prelude.Int)
identityUsage_datasetCount = Lens.lens (\IdentityUsage' {datasetCount} -> datasetCount) (\s@IdentityUsage' {} a -> s {datasetCount = a} :: IdentityUsage)

-- | Total data storage for this identity.
identityUsage_dataStorage :: Lens.Lens' IdentityUsage (Prelude.Maybe Prelude.Integer)
identityUsage_dataStorage = Lens.lens (\IdentityUsage' {dataStorage} -> dataStorage) (\s@IdentityUsage' {} a -> s {dataStorage = a} :: IdentityUsage)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
identityUsage_identityId :: Lens.Lens' IdentityUsage (Prelude.Maybe Prelude.Text)
identityUsage_identityId = Lens.lens (\IdentityUsage' {identityId} -> identityId) (\s@IdentityUsage' {} a -> s {identityId = a} :: IdentityUsage)

instance Prelude.FromJSON IdentityUsage where
  parseJSON =
    Prelude.withObject
      "IdentityUsage"
      ( \x ->
          IdentityUsage'
            Prelude.<$> (x Prelude..:? "LastModifiedDate")
            Prelude.<*> (x Prelude..:? "IdentityPoolId")
            Prelude.<*> (x Prelude..:? "DatasetCount")
            Prelude.<*> (x Prelude..:? "DataStorage")
            Prelude.<*> (x Prelude..:? "IdentityId")
      )

instance Prelude.Hashable IdentityUsage

instance Prelude.NFData IdentityUsage
