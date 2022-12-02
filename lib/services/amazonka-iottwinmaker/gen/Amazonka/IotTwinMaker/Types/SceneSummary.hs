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
-- Module      : Amazonka.IotTwinMaker.Types.SceneSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.SceneSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about a scene.
--
-- /See:/ 'newSceneSummary' smart constructor.
data SceneSummary = SceneSummary'
  { -- | The scene description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the scene.
    sceneId :: Prelude.Text,
    -- | The relative path that specifies the location of the content definition
    -- file.
    contentLocation :: Prelude.Text,
    -- | The ARN of the scene.
    arn :: Prelude.Text,
    -- | The date and time when the scene was created.
    creationDateTime :: Data.POSIX,
    -- | The date and time when the scene was last updated.
    updateDateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SceneSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'sceneSummary_description' - The scene description.
--
-- 'sceneId', 'sceneSummary_sceneId' - The ID of the scene.
--
-- 'contentLocation', 'sceneSummary_contentLocation' - The relative path that specifies the location of the content definition
-- file.
--
-- 'arn', 'sceneSummary_arn' - The ARN of the scene.
--
-- 'creationDateTime', 'sceneSummary_creationDateTime' - The date and time when the scene was created.
--
-- 'updateDateTime', 'sceneSummary_updateDateTime' - The date and time when the scene was last updated.
newSceneSummary ::
  -- | 'sceneId'
  Prelude.Text ->
  -- | 'contentLocation'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  SceneSummary
newSceneSummary
  pSceneId_
  pContentLocation_
  pArn_
  pCreationDateTime_
  pUpdateDateTime_ =
    SceneSummary'
      { description = Prelude.Nothing,
        sceneId = pSceneId_,
        contentLocation = pContentLocation_,
        arn = pArn_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_,
        updateDateTime = Data._Time Lens.# pUpdateDateTime_
      }

-- | The scene description.
sceneSummary_description :: Lens.Lens' SceneSummary (Prelude.Maybe Prelude.Text)
sceneSummary_description = Lens.lens (\SceneSummary' {description} -> description) (\s@SceneSummary' {} a -> s {description = a} :: SceneSummary)

-- | The ID of the scene.
sceneSummary_sceneId :: Lens.Lens' SceneSummary Prelude.Text
sceneSummary_sceneId = Lens.lens (\SceneSummary' {sceneId} -> sceneId) (\s@SceneSummary' {} a -> s {sceneId = a} :: SceneSummary)

-- | The relative path that specifies the location of the content definition
-- file.
sceneSummary_contentLocation :: Lens.Lens' SceneSummary Prelude.Text
sceneSummary_contentLocation = Lens.lens (\SceneSummary' {contentLocation} -> contentLocation) (\s@SceneSummary' {} a -> s {contentLocation = a} :: SceneSummary)

-- | The ARN of the scene.
sceneSummary_arn :: Lens.Lens' SceneSummary Prelude.Text
sceneSummary_arn = Lens.lens (\SceneSummary' {arn} -> arn) (\s@SceneSummary' {} a -> s {arn = a} :: SceneSummary)

-- | The date and time when the scene was created.
sceneSummary_creationDateTime :: Lens.Lens' SceneSummary Prelude.UTCTime
sceneSummary_creationDateTime = Lens.lens (\SceneSummary' {creationDateTime} -> creationDateTime) (\s@SceneSummary' {} a -> s {creationDateTime = a} :: SceneSummary) Prelude.. Data._Time

-- | The date and time when the scene was last updated.
sceneSummary_updateDateTime :: Lens.Lens' SceneSummary Prelude.UTCTime
sceneSummary_updateDateTime = Lens.lens (\SceneSummary' {updateDateTime} -> updateDateTime) (\s@SceneSummary' {} a -> s {updateDateTime = a} :: SceneSummary) Prelude.. Data._Time

instance Data.FromJSON SceneSummary where
  parseJSON =
    Data.withObject
      "SceneSummary"
      ( \x ->
          SceneSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..: "sceneId")
            Prelude.<*> (x Data..: "contentLocation")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "creationDateTime")
            Prelude.<*> (x Data..: "updateDateTime")
      )

instance Prelude.Hashable SceneSummary where
  hashWithSalt _salt SceneSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sceneId
      `Prelude.hashWithSalt` contentLocation
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` updateDateTime

instance Prelude.NFData SceneSummary where
  rnf SceneSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf sceneId
      `Prelude.seq` Prelude.rnf contentLocation
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf updateDateTime
