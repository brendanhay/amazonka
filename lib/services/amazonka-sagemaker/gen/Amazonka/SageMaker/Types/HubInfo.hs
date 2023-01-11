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
-- Module      : Amazonka.SageMaker.Types.HubInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HubInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HubStatus

-- | Information about a hub.
--
-- /See:/ 'newHubInfo' smart constructor.
data HubInfo = HubInfo'
  { -- | A description of the hub.
    hubDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the hub.
    hubDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The searchable keywords for the hub.
    hubSearchKeywords :: Prelude.Maybe [Prelude.Text],
    -- | The name of the hub.
    hubName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the hub.
    hubArn :: Prelude.Text,
    -- | The status of the hub.
    hubStatus :: HubStatus,
    -- | The date and time that the hub was created.
    creationTime :: Data.POSIX,
    -- | The date and time that the hub was last modified.
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HubInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hubDescription', 'hubInfo_hubDescription' - A description of the hub.
--
-- 'hubDisplayName', 'hubInfo_hubDisplayName' - The display name of the hub.
--
-- 'hubSearchKeywords', 'hubInfo_hubSearchKeywords' - The searchable keywords for the hub.
--
-- 'hubName', 'hubInfo_hubName' - The name of the hub.
--
-- 'hubArn', 'hubInfo_hubArn' - The Amazon Resource Name (ARN) of the hub.
--
-- 'hubStatus', 'hubInfo_hubStatus' - The status of the hub.
--
-- 'creationTime', 'hubInfo_creationTime' - The date and time that the hub was created.
--
-- 'lastModifiedTime', 'hubInfo_lastModifiedTime' - The date and time that the hub was last modified.
newHubInfo ::
  -- | 'hubName'
  Prelude.Text ->
  -- | 'hubArn'
  Prelude.Text ->
  -- | 'hubStatus'
  HubStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  HubInfo
newHubInfo
  pHubName_
  pHubArn_
  pHubStatus_
  pCreationTime_
  pLastModifiedTime_ =
    HubInfo'
      { hubDescription = Prelude.Nothing,
        hubDisplayName = Prelude.Nothing,
        hubSearchKeywords = Prelude.Nothing,
        hubName = pHubName_,
        hubArn = pHubArn_,
        hubStatus = pHubStatus_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | A description of the hub.
hubInfo_hubDescription :: Lens.Lens' HubInfo (Prelude.Maybe Prelude.Text)
hubInfo_hubDescription = Lens.lens (\HubInfo' {hubDescription} -> hubDescription) (\s@HubInfo' {} a -> s {hubDescription = a} :: HubInfo)

-- | The display name of the hub.
hubInfo_hubDisplayName :: Lens.Lens' HubInfo (Prelude.Maybe Prelude.Text)
hubInfo_hubDisplayName = Lens.lens (\HubInfo' {hubDisplayName} -> hubDisplayName) (\s@HubInfo' {} a -> s {hubDisplayName = a} :: HubInfo)

-- | The searchable keywords for the hub.
hubInfo_hubSearchKeywords :: Lens.Lens' HubInfo (Prelude.Maybe [Prelude.Text])
hubInfo_hubSearchKeywords = Lens.lens (\HubInfo' {hubSearchKeywords} -> hubSearchKeywords) (\s@HubInfo' {} a -> s {hubSearchKeywords = a} :: HubInfo) Prelude.. Lens.mapping Lens.coerced

-- | The name of the hub.
hubInfo_hubName :: Lens.Lens' HubInfo Prelude.Text
hubInfo_hubName = Lens.lens (\HubInfo' {hubName} -> hubName) (\s@HubInfo' {} a -> s {hubName = a} :: HubInfo)

-- | The Amazon Resource Name (ARN) of the hub.
hubInfo_hubArn :: Lens.Lens' HubInfo Prelude.Text
hubInfo_hubArn = Lens.lens (\HubInfo' {hubArn} -> hubArn) (\s@HubInfo' {} a -> s {hubArn = a} :: HubInfo)

-- | The status of the hub.
hubInfo_hubStatus :: Lens.Lens' HubInfo HubStatus
hubInfo_hubStatus = Lens.lens (\HubInfo' {hubStatus} -> hubStatus) (\s@HubInfo' {} a -> s {hubStatus = a} :: HubInfo)

-- | The date and time that the hub was created.
hubInfo_creationTime :: Lens.Lens' HubInfo Prelude.UTCTime
hubInfo_creationTime = Lens.lens (\HubInfo' {creationTime} -> creationTime) (\s@HubInfo' {} a -> s {creationTime = a} :: HubInfo) Prelude.. Data._Time

-- | The date and time that the hub was last modified.
hubInfo_lastModifiedTime :: Lens.Lens' HubInfo Prelude.UTCTime
hubInfo_lastModifiedTime = Lens.lens (\HubInfo' {lastModifiedTime} -> lastModifiedTime) (\s@HubInfo' {} a -> s {lastModifiedTime = a} :: HubInfo) Prelude.. Data._Time

instance Data.FromJSON HubInfo where
  parseJSON =
    Data.withObject
      "HubInfo"
      ( \x ->
          HubInfo'
            Prelude.<$> (x Data..:? "HubDescription")
            Prelude.<*> (x Data..:? "HubDisplayName")
            Prelude.<*> ( x Data..:? "HubSearchKeywords"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "HubName")
            Prelude.<*> (x Data..: "HubArn")
            Prelude.<*> (x Data..: "HubStatus")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
      )

instance Prelude.Hashable HubInfo where
  hashWithSalt _salt HubInfo' {..} =
    _salt `Prelude.hashWithSalt` hubDescription
      `Prelude.hashWithSalt` hubDisplayName
      `Prelude.hashWithSalt` hubSearchKeywords
      `Prelude.hashWithSalt` hubName
      `Prelude.hashWithSalt` hubArn
      `Prelude.hashWithSalt` hubStatus
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData HubInfo where
  rnf HubInfo' {..} =
    Prelude.rnf hubDescription
      `Prelude.seq` Prelude.rnf hubDisplayName
      `Prelude.seq` Prelude.rnf hubSearchKeywords
      `Prelude.seq` Prelude.rnf hubName
      `Prelude.seq` Prelude.rnf hubArn
      `Prelude.seq` Prelude.rnf hubStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
