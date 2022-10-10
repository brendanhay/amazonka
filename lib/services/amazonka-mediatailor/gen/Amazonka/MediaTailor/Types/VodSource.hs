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
-- Module      : Amazonka.MediaTailor.Types.VodSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.VodSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.HttpPackageConfiguration
import qualified Amazonka.Prelude as Prelude

-- | VOD source configuration parameters.
--
-- /See:/ 'newVodSource' smart constructor.
data VodSource = VodSource'
  { -- | The tags assigned to the VOD source.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The timestamp that indicates when the VOD source was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The timestamp that indicates when the VOD source was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the VOD source.
    vodSourceName :: Prelude.Text,
    -- | The name of the source location that the VOD source is associated with.
    sourceLocationName :: Prelude.Text,
    -- | The HTTP package configurations for the VOD source.
    httpPackageConfigurations :: [HttpPackageConfiguration],
    -- | The ARN for the VOD source.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VodSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'vodSource_tags' - The tags assigned to the VOD source.
--
-- 'lastModifiedTime', 'vodSource_lastModifiedTime' - The timestamp that indicates when the VOD source was last modified.
--
-- 'creationTime', 'vodSource_creationTime' - The timestamp that indicates when the VOD source was created.
--
-- 'vodSourceName', 'vodSource_vodSourceName' - The name of the VOD source.
--
-- 'sourceLocationName', 'vodSource_sourceLocationName' - The name of the source location that the VOD source is associated with.
--
-- 'httpPackageConfigurations', 'vodSource_httpPackageConfigurations' - The HTTP package configurations for the VOD source.
--
-- 'arn', 'vodSource_arn' - The ARN for the VOD source.
newVodSource ::
  -- | 'vodSourceName'
  Prelude.Text ->
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  VodSource
newVodSource
  pVodSourceName_
  pSourceLocationName_
  pArn_ =
    VodSource'
      { tags = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        creationTime = Prelude.Nothing,
        vodSourceName = pVodSourceName_,
        sourceLocationName = pSourceLocationName_,
        httpPackageConfigurations = Prelude.mempty,
        arn = pArn_
      }

-- | The tags assigned to the VOD source.
vodSource_tags :: Lens.Lens' VodSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
vodSource_tags = Lens.lens (\VodSource' {tags} -> tags) (\s@VodSource' {} a -> s {tags = a} :: VodSource) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp that indicates when the VOD source was last modified.
vodSource_lastModifiedTime :: Lens.Lens' VodSource (Prelude.Maybe Prelude.UTCTime)
vodSource_lastModifiedTime = Lens.lens (\VodSource' {lastModifiedTime} -> lastModifiedTime) (\s@VodSource' {} a -> s {lastModifiedTime = a} :: VodSource) Prelude.. Lens.mapping Core._Time

-- | The timestamp that indicates when the VOD source was created.
vodSource_creationTime :: Lens.Lens' VodSource (Prelude.Maybe Prelude.UTCTime)
vodSource_creationTime = Lens.lens (\VodSource' {creationTime} -> creationTime) (\s@VodSource' {} a -> s {creationTime = a} :: VodSource) Prelude.. Lens.mapping Core._Time

-- | The name of the VOD source.
vodSource_vodSourceName :: Lens.Lens' VodSource Prelude.Text
vodSource_vodSourceName = Lens.lens (\VodSource' {vodSourceName} -> vodSourceName) (\s@VodSource' {} a -> s {vodSourceName = a} :: VodSource)

-- | The name of the source location that the VOD source is associated with.
vodSource_sourceLocationName :: Lens.Lens' VodSource Prelude.Text
vodSource_sourceLocationName = Lens.lens (\VodSource' {sourceLocationName} -> sourceLocationName) (\s@VodSource' {} a -> s {sourceLocationName = a} :: VodSource)

-- | The HTTP package configurations for the VOD source.
vodSource_httpPackageConfigurations :: Lens.Lens' VodSource [HttpPackageConfiguration]
vodSource_httpPackageConfigurations = Lens.lens (\VodSource' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@VodSource' {} a -> s {httpPackageConfigurations = a} :: VodSource) Prelude.. Lens.coerced

-- | The ARN for the VOD source.
vodSource_arn :: Lens.Lens' VodSource Prelude.Text
vodSource_arn = Lens.lens (\VodSource' {arn} -> arn) (\s@VodSource' {} a -> s {arn = a} :: VodSource)

instance Core.FromJSON VodSource where
  parseJSON =
    Core.withObject
      "VodSource"
      ( \x ->
          VodSource'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..: "VodSourceName")
            Prelude.<*> (x Core..: "SourceLocationName")
            Prelude.<*> ( x Core..:? "HttpPackageConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "Arn")
      )

instance Prelude.Hashable VodSource where
  hashWithSalt _salt VodSource' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` vodSourceName
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` httpPackageConfigurations
      `Prelude.hashWithSalt` arn

instance Prelude.NFData VodSource where
  rnf VodSource' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf arn
