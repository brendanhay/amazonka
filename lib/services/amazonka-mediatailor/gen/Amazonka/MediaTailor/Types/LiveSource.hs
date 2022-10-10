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
-- Module      : Amazonka.MediaTailor.Types.LiveSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.LiveSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.HttpPackageConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Live source configuration parameters.
--
-- /See:/ 'newLiveSource' smart constructor.
data LiveSource = LiveSource'
  { -- | The tags assigned to the live source.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The timestamp that indicates when the live source was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The timestamp that indicates when the live source was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the source location.
    sourceLocationName :: Prelude.Text,
    -- | The name that\'s used to refer to a live source.
    liveSourceName :: Prelude.Text,
    -- | The HTTP package configurations for the live source.
    httpPackageConfigurations :: [HttpPackageConfiguration],
    -- | The ARN for the live source.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LiveSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'liveSource_tags' - The tags assigned to the live source.
--
-- 'lastModifiedTime', 'liveSource_lastModifiedTime' - The timestamp that indicates when the live source was last modified.
--
-- 'creationTime', 'liveSource_creationTime' - The timestamp that indicates when the live source was created.
--
-- 'sourceLocationName', 'liveSource_sourceLocationName' - The name of the source location.
--
-- 'liveSourceName', 'liveSource_liveSourceName' - The name that\'s used to refer to a live source.
--
-- 'httpPackageConfigurations', 'liveSource_httpPackageConfigurations' - The HTTP package configurations for the live source.
--
-- 'arn', 'liveSource_arn' - The ARN for the live source.
newLiveSource ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'liveSourceName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  LiveSource
newLiveSource
  pSourceLocationName_
  pLiveSourceName_
  pArn_ =
    LiveSource'
      { tags = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        creationTime = Prelude.Nothing,
        sourceLocationName = pSourceLocationName_,
        liveSourceName = pLiveSourceName_,
        httpPackageConfigurations = Prelude.mempty,
        arn = pArn_
      }

-- | The tags assigned to the live source.
liveSource_tags :: Lens.Lens' LiveSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
liveSource_tags = Lens.lens (\LiveSource' {tags} -> tags) (\s@LiveSource' {} a -> s {tags = a} :: LiveSource) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp that indicates when the live source was last modified.
liveSource_lastModifiedTime :: Lens.Lens' LiveSource (Prelude.Maybe Prelude.UTCTime)
liveSource_lastModifiedTime = Lens.lens (\LiveSource' {lastModifiedTime} -> lastModifiedTime) (\s@LiveSource' {} a -> s {lastModifiedTime = a} :: LiveSource) Prelude.. Lens.mapping Core._Time

-- | The timestamp that indicates when the live source was created.
liveSource_creationTime :: Lens.Lens' LiveSource (Prelude.Maybe Prelude.UTCTime)
liveSource_creationTime = Lens.lens (\LiveSource' {creationTime} -> creationTime) (\s@LiveSource' {} a -> s {creationTime = a} :: LiveSource) Prelude.. Lens.mapping Core._Time

-- | The name of the source location.
liveSource_sourceLocationName :: Lens.Lens' LiveSource Prelude.Text
liveSource_sourceLocationName = Lens.lens (\LiveSource' {sourceLocationName} -> sourceLocationName) (\s@LiveSource' {} a -> s {sourceLocationName = a} :: LiveSource)

-- | The name that\'s used to refer to a live source.
liveSource_liveSourceName :: Lens.Lens' LiveSource Prelude.Text
liveSource_liveSourceName = Lens.lens (\LiveSource' {liveSourceName} -> liveSourceName) (\s@LiveSource' {} a -> s {liveSourceName = a} :: LiveSource)

-- | The HTTP package configurations for the live source.
liveSource_httpPackageConfigurations :: Lens.Lens' LiveSource [HttpPackageConfiguration]
liveSource_httpPackageConfigurations = Lens.lens (\LiveSource' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@LiveSource' {} a -> s {httpPackageConfigurations = a} :: LiveSource) Prelude.. Lens.coerced

-- | The ARN for the live source.
liveSource_arn :: Lens.Lens' LiveSource Prelude.Text
liveSource_arn = Lens.lens (\LiveSource' {arn} -> arn) (\s@LiveSource' {} a -> s {arn = a} :: LiveSource)

instance Core.FromJSON LiveSource where
  parseJSON =
    Core.withObject
      "LiveSource"
      ( \x ->
          LiveSource'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..: "SourceLocationName")
            Prelude.<*> (x Core..: "LiveSourceName")
            Prelude.<*> ( x Core..:? "HttpPackageConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "Arn")
      )

instance Prelude.Hashable LiveSource where
  hashWithSalt _salt LiveSource' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` liveSourceName
      `Prelude.hashWithSalt` httpPackageConfigurations
      `Prelude.hashWithSalt` arn

instance Prelude.NFData LiveSource where
  rnf LiveSource' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf arn
