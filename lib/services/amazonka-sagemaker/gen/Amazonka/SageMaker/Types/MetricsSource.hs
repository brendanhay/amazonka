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
-- Module      : Amazonka.SageMaker.Types.MetricsSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MetricsSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the metrics source.
--
-- /See:/ 'newMetricsSource' smart constructor.
data MetricsSource = MetricsSource'
  { -- | The hash key used for the metrics source.
    contentDigest :: Prelude.Maybe Prelude.Text,
    -- | The metric source content type.
    contentType :: Prelude.Text,
    -- | The S3 URI for the metrics source.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricsSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentDigest', 'metricsSource_contentDigest' - The hash key used for the metrics source.
--
-- 'contentType', 'metricsSource_contentType' - The metric source content type.
--
-- 's3Uri', 'metricsSource_s3Uri' - The S3 URI for the metrics source.
newMetricsSource ::
  -- | 'contentType'
  Prelude.Text ->
  -- | 's3Uri'
  Prelude.Text ->
  MetricsSource
newMetricsSource pContentType_ pS3Uri_ =
  MetricsSource'
    { contentDigest = Prelude.Nothing,
      contentType = pContentType_,
      s3Uri = pS3Uri_
    }

-- | The hash key used for the metrics source.
metricsSource_contentDigest :: Lens.Lens' MetricsSource (Prelude.Maybe Prelude.Text)
metricsSource_contentDigest = Lens.lens (\MetricsSource' {contentDigest} -> contentDigest) (\s@MetricsSource' {} a -> s {contentDigest = a} :: MetricsSource)

-- | The metric source content type.
metricsSource_contentType :: Lens.Lens' MetricsSource Prelude.Text
metricsSource_contentType = Lens.lens (\MetricsSource' {contentType} -> contentType) (\s@MetricsSource' {} a -> s {contentType = a} :: MetricsSource)

-- | The S3 URI for the metrics source.
metricsSource_s3Uri :: Lens.Lens' MetricsSource Prelude.Text
metricsSource_s3Uri = Lens.lens (\MetricsSource' {s3Uri} -> s3Uri) (\s@MetricsSource' {} a -> s {s3Uri = a} :: MetricsSource)

instance Data.FromJSON MetricsSource where
  parseJSON =
    Data.withObject
      "MetricsSource"
      ( \x ->
          MetricsSource'
            Prelude.<$> (x Data..:? "ContentDigest")
            Prelude.<*> (x Data..: "ContentType")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable MetricsSource where
  hashWithSalt _salt MetricsSource' {..} =
    _salt `Prelude.hashWithSalt` contentDigest
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData MetricsSource where
  rnf MetricsSource' {..} =
    Prelude.rnf contentDigest
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON MetricsSource where
  toJSON MetricsSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentDigest" Data..=) Prelude.<$> contentDigest,
            Prelude.Just ("ContentType" Data..= contentType),
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
