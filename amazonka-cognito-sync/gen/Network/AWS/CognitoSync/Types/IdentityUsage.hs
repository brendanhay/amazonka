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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Usage information for the identity.
--
-- /See:/ 'newIdentityUsage' smart constructor.
data IdentityUsage = IdentityUsage'
  { -- | Date on which the identity was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Maybe Core.Text,
    -- | Number of datasets for the identity.
    datasetCount :: Core.Maybe Core.Int,
    -- | Total data storage for this identity.
    dataStorage :: Core.Maybe Core.Integer,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { lastModifiedDate = Core.Nothing,
      identityPoolId = Core.Nothing,
      datasetCount = Core.Nothing,
      dataStorage = Core.Nothing,
      identityId = Core.Nothing
    }

-- | Date on which the identity was last modified.
identityUsage_lastModifiedDate :: Lens.Lens' IdentityUsage (Core.Maybe Core.UTCTime)
identityUsage_lastModifiedDate = Lens.lens (\IdentityUsage' {lastModifiedDate} -> lastModifiedDate) (\s@IdentityUsage' {} a -> s {lastModifiedDate = a} :: IdentityUsage) Core.. Lens.mapping Core._Time

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
identityUsage_identityPoolId :: Lens.Lens' IdentityUsage (Core.Maybe Core.Text)
identityUsage_identityPoolId = Lens.lens (\IdentityUsage' {identityPoolId} -> identityPoolId) (\s@IdentityUsage' {} a -> s {identityPoolId = a} :: IdentityUsage)

-- | Number of datasets for the identity.
identityUsage_datasetCount :: Lens.Lens' IdentityUsage (Core.Maybe Core.Int)
identityUsage_datasetCount = Lens.lens (\IdentityUsage' {datasetCount} -> datasetCount) (\s@IdentityUsage' {} a -> s {datasetCount = a} :: IdentityUsage)

-- | Total data storage for this identity.
identityUsage_dataStorage :: Lens.Lens' IdentityUsage (Core.Maybe Core.Integer)
identityUsage_dataStorage = Lens.lens (\IdentityUsage' {dataStorage} -> dataStorage) (\s@IdentityUsage' {} a -> s {dataStorage = a} :: IdentityUsage)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
identityUsage_identityId :: Lens.Lens' IdentityUsage (Core.Maybe Core.Text)
identityUsage_identityId = Lens.lens (\IdentityUsage' {identityId} -> identityId) (\s@IdentityUsage' {} a -> s {identityId = a} :: IdentityUsage)

instance Core.FromJSON IdentityUsage where
  parseJSON =
    Core.withObject
      "IdentityUsage"
      ( \x ->
          IdentityUsage'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "IdentityPoolId")
            Core.<*> (x Core..:? "DatasetCount")
            Core.<*> (x Core..:? "DataStorage")
            Core.<*> (x Core..:? "IdentityId")
      )

instance Core.Hashable IdentityUsage

instance Core.NFData IdentityUsage
