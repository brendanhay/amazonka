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
-- Module      : Network.AWS.CognitoSync.Types.IdentityPoolUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.IdentityPoolUsage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Usage information for the identity pool.
--
-- /See:/ 'newIdentityPoolUsage' smart constructor.
data IdentityPoolUsage = IdentityPoolUsage'
  { -- | Date on which the identity pool was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Maybe Core.Text,
    -- | Number of sync sessions for the identity pool.
    syncSessionsCount :: Core.Maybe Core.Integer,
    -- | Data storage information for the identity pool.
    dataStorage :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IdentityPoolUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'identityPoolUsage_lastModifiedDate' - Date on which the identity pool was last modified.
--
-- 'identityPoolId', 'identityPoolUsage_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'syncSessionsCount', 'identityPoolUsage_syncSessionsCount' - Number of sync sessions for the identity pool.
--
-- 'dataStorage', 'identityPoolUsage_dataStorage' - Data storage information for the identity pool.
newIdentityPoolUsage ::
  IdentityPoolUsage
newIdentityPoolUsage =
  IdentityPoolUsage'
    { lastModifiedDate = Core.Nothing,
      identityPoolId = Core.Nothing,
      syncSessionsCount = Core.Nothing,
      dataStorage = Core.Nothing
    }

-- | Date on which the identity pool was last modified.
identityPoolUsage_lastModifiedDate :: Lens.Lens' IdentityPoolUsage (Core.Maybe Core.UTCTime)
identityPoolUsage_lastModifiedDate = Lens.lens (\IdentityPoolUsage' {lastModifiedDate} -> lastModifiedDate) (\s@IdentityPoolUsage' {} a -> s {lastModifiedDate = a} :: IdentityPoolUsage) Core.. Lens.mapping Core._Time

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
identityPoolUsage_identityPoolId :: Lens.Lens' IdentityPoolUsage (Core.Maybe Core.Text)
identityPoolUsage_identityPoolId = Lens.lens (\IdentityPoolUsage' {identityPoolId} -> identityPoolId) (\s@IdentityPoolUsage' {} a -> s {identityPoolId = a} :: IdentityPoolUsage)

-- | Number of sync sessions for the identity pool.
identityPoolUsage_syncSessionsCount :: Lens.Lens' IdentityPoolUsage (Core.Maybe Core.Integer)
identityPoolUsage_syncSessionsCount = Lens.lens (\IdentityPoolUsage' {syncSessionsCount} -> syncSessionsCount) (\s@IdentityPoolUsage' {} a -> s {syncSessionsCount = a} :: IdentityPoolUsage)

-- | Data storage information for the identity pool.
identityPoolUsage_dataStorage :: Lens.Lens' IdentityPoolUsage (Core.Maybe Core.Integer)
identityPoolUsage_dataStorage = Lens.lens (\IdentityPoolUsage' {dataStorage} -> dataStorage) (\s@IdentityPoolUsage' {} a -> s {dataStorage = a} :: IdentityPoolUsage)

instance Core.FromJSON IdentityPoolUsage where
  parseJSON =
    Core.withObject
      "IdentityPoolUsage"
      ( \x ->
          IdentityPoolUsage'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "IdentityPoolId")
            Core.<*> (x Core..:? "SyncSessionsCount")
            Core.<*> (x Core..:? "DataStorage")
      )

instance Core.Hashable IdentityPoolUsage

instance Core.NFData IdentityPoolUsage
