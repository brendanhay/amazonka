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
-- Module      : Amazonka.SageMaker.Types.HubContentInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HubContentInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HubContentStatus
import Amazonka.SageMaker.Types.HubContentType

-- | Information about hub content.
--
-- /See:/ 'newHubContentInfo' smart constructor.
data HubContentInfo = HubContentInfo'
  { -- | A description of the hub content.
    hubContentDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the hub content.
    hubContentDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The searchable keywords for the hub content.
    hubContentSearchKeywords :: Prelude.Maybe [Prelude.Text],
    -- | The name of the hub content.
    hubContentName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the hub content.
    hubContentArn :: Prelude.Text,
    -- | The version of the hub content.
    hubContentVersion :: Prelude.Text,
    -- | The type of hub content.
    hubContentType :: HubContentType,
    -- | The version of the hub content document schema.
    documentSchemaVersion :: Prelude.Text,
    -- | The status of the hub content.
    hubContentStatus :: HubContentStatus,
    -- | The date and time that the hub content was created.
    creationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HubContentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hubContentDescription', 'hubContentInfo_hubContentDescription' - A description of the hub content.
--
-- 'hubContentDisplayName', 'hubContentInfo_hubContentDisplayName' - The display name of the hub content.
--
-- 'hubContentSearchKeywords', 'hubContentInfo_hubContentSearchKeywords' - The searchable keywords for the hub content.
--
-- 'hubContentName', 'hubContentInfo_hubContentName' - The name of the hub content.
--
-- 'hubContentArn', 'hubContentInfo_hubContentArn' - The Amazon Resource Name (ARN) of the hub content.
--
-- 'hubContentVersion', 'hubContentInfo_hubContentVersion' - The version of the hub content.
--
-- 'hubContentType', 'hubContentInfo_hubContentType' - The type of hub content.
--
-- 'documentSchemaVersion', 'hubContentInfo_documentSchemaVersion' - The version of the hub content document schema.
--
-- 'hubContentStatus', 'hubContentInfo_hubContentStatus' - The status of the hub content.
--
-- 'creationTime', 'hubContentInfo_creationTime' - The date and time that the hub content was created.
newHubContentInfo ::
  -- | 'hubContentName'
  Prelude.Text ->
  -- | 'hubContentArn'
  Prelude.Text ->
  -- | 'hubContentVersion'
  Prelude.Text ->
  -- | 'hubContentType'
  HubContentType ->
  -- | 'documentSchemaVersion'
  Prelude.Text ->
  -- | 'hubContentStatus'
  HubContentStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  HubContentInfo
newHubContentInfo
  pHubContentName_
  pHubContentArn_
  pHubContentVersion_
  pHubContentType_
  pDocumentSchemaVersion_
  pHubContentStatus_
  pCreationTime_ =
    HubContentInfo'
      { hubContentDescription =
          Prelude.Nothing,
        hubContentDisplayName = Prelude.Nothing,
        hubContentSearchKeywords = Prelude.Nothing,
        hubContentName = pHubContentName_,
        hubContentArn = pHubContentArn_,
        hubContentVersion = pHubContentVersion_,
        hubContentType = pHubContentType_,
        documentSchemaVersion = pDocumentSchemaVersion_,
        hubContentStatus = pHubContentStatus_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | A description of the hub content.
hubContentInfo_hubContentDescription :: Lens.Lens' HubContentInfo (Prelude.Maybe Prelude.Text)
hubContentInfo_hubContentDescription = Lens.lens (\HubContentInfo' {hubContentDescription} -> hubContentDescription) (\s@HubContentInfo' {} a -> s {hubContentDescription = a} :: HubContentInfo)

-- | The display name of the hub content.
hubContentInfo_hubContentDisplayName :: Lens.Lens' HubContentInfo (Prelude.Maybe Prelude.Text)
hubContentInfo_hubContentDisplayName = Lens.lens (\HubContentInfo' {hubContentDisplayName} -> hubContentDisplayName) (\s@HubContentInfo' {} a -> s {hubContentDisplayName = a} :: HubContentInfo)

-- | The searchable keywords for the hub content.
hubContentInfo_hubContentSearchKeywords :: Lens.Lens' HubContentInfo (Prelude.Maybe [Prelude.Text])
hubContentInfo_hubContentSearchKeywords = Lens.lens (\HubContentInfo' {hubContentSearchKeywords} -> hubContentSearchKeywords) (\s@HubContentInfo' {} a -> s {hubContentSearchKeywords = a} :: HubContentInfo) Prelude.. Lens.mapping Lens.coerced

-- | The name of the hub content.
hubContentInfo_hubContentName :: Lens.Lens' HubContentInfo Prelude.Text
hubContentInfo_hubContentName = Lens.lens (\HubContentInfo' {hubContentName} -> hubContentName) (\s@HubContentInfo' {} a -> s {hubContentName = a} :: HubContentInfo)

-- | The Amazon Resource Name (ARN) of the hub content.
hubContentInfo_hubContentArn :: Lens.Lens' HubContentInfo Prelude.Text
hubContentInfo_hubContentArn = Lens.lens (\HubContentInfo' {hubContentArn} -> hubContentArn) (\s@HubContentInfo' {} a -> s {hubContentArn = a} :: HubContentInfo)

-- | The version of the hub content.
hubContentInfo_hubContentVersion :: Lens.Lens' HubContentInfo Prelude.Text
hubContentInfo_hubContentVersion = Lens.lens (\HubContentInfo' {hubContentVersion} -> hubContentVersion) (\s@HubContentInfo' {} a -> s {hubContentVersion = a} :: HubContentInfo)

-- | The type of hub content.
hubContentInfo_hubContentType :: Lens.Lens' HubContentInfo HubContentType
hubContentInfo_hubContentType = Lens.lens (\HubContentInfo' {hubContentType} -> hubContentType) (\s@HubContentInfo' {} a -> s {hubContentType = a} :: HubContentInfo)

-- | The version of the hub content document schema.
hubContentInfo_documentSchemaVersion :: Lens.Lens' HubContentInfo Prelude.Text
hubContentInfo_documentSchemaVersion = Lens.lens (\HubContentInfo' {documentSchemaVersion} -> documentSchemaVersion) (\s@HubContentInfo' {} a -> s {documentSchemaVersion = a} :: HubContentInfo)

-- | The status of the hub content.
hubContentInfo_hubContentStatus :: Lens.Lens' HubContentInfo HubContentStatus
hubContentInfo_hubContentStatus = Lens.lens (\HubContentInfo' {hubContentStatus} -> hubContentStatus) (\s@HubContentInfo' {} a -> s {hubContentStatus = a} :: HubContentInfo)

-- | The date and time that the hub content was created.
hubContentInfo_creationTime :: Lens.Lens' HubContentInfo Prelude.UTCTime
hubContentInfo_creationTime = Lens.lens (\HubContentInfo' {creationTime} -> creationTime) (\s@HubContentInfo' {} a -> s {creationTime = a} :: HubContentInfo) Prelude.. Data._Time

instance Data.FromJSON HubContentInfo where
  parseJSON =
    Data.withObject
      "HubContentInfo"
      ( \x ->
          HubContentInfo'
            Prelude.<$> (x Data..:? "HubContentDescription")
            Prelude.<*> (x Data..:? "HubContentDisplayName")
            Prelude.<*> ( x
                            Data..:? "HubContentSearchKeywords"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "HubContentName")
            Prelude.<*> (x Data..: "HubContentArn")
            Prelude.<*> (x Data..: "HubContentVersion")
            Prelude.<*> (x Data..: "HubContentType")
            Prelude.<*> (x Data..: "DocumentSchemaVersion")
            Prelude.<*> (x Data..: "HubContentStatus")
            Prelude.<*> (x Data..: "CreationTime")
      )

instance Prelude.Hashable HubContentInfo where
  hashWithSalt _salt HubContentInfo' {..} =
    _salt
      `Prelude.hashWithSalt` hubContentDescription
      `Prelude.hashWithSalt` hubContentDisplayName
      `Prelude.hashWithSalt` hubContentSearchKeywords
      `Prelude.hashWithSalt` hubContentName
      `Prelude.hashWithSalt` hubContentArn
      `Prelude.hashWithSalt` hubContentVersion
      `Prelude.hashWithSalt` hubContentType
      `Prelude.hashWithSalt` documentSchemaVersion
      `Prelude.hashWithSalt` hubContentStatus
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData HubContentInfo where
  rnf HubContentInfo' {..} =
    Prelude.rnf hubContentDescription
      `Prelude.seq` Prelude.rnf hubContentDisplayName
      `Prelude.seq` Prelude.rnf hubContentSearchKeywords
      `Prelude.seq` Prelude.rnf hubContentName
      `Prelude.seq` Prelude.rnf hubContentArn
      `Prelude.seq` Prelude.rnf hubContentVersion
      `Prelude.seq` Prelude.rnf hubContentType
      `Prelude.seq` Prelude.rnf documentSchemaVersion
      `Prelude.seq` Prelude.rnf hubContentStatus
      `Prelude.seq` Prelude.rnf creationTime
