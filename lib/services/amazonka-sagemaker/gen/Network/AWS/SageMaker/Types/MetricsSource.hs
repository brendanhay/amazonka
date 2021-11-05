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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MetricsSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newMetricsSource' smart constructor.
data MetricsSource = MetricsSource'
  { contentDigest :: Prelude.Maybe Prelude.Text,
    contentType :: Prelude.Text,
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
-- 'contentDigest', 'metricsSource_contentDigest' -
--
-- 'contentType', 'metricsSource_contentType' -
--
-- 's3Uri', 'metricsSource_s3Uri' -
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

-- |
metricsSource_contentDigest :: Lens.Lens' MetricsSource (Prelude.Maybe Prelude.Text)
metricsSource_contentDigest = Lens.lens (\MetricsSource' {contentDigest} -> contentDigest) (\s@MetricsSource' {} a -> s {contentDigest = a} :: MetricsSource)

-- |
metricsSource_contentType :: Lens.Lens' MetricsSource Prelude.Text
metricsSource_contentType = Lens.lens (\MetricsSource' {contentType} -> contentType) (\s@MetricsSource' {} a -> s {contentType = a} :: MetricsSource)

-- |
metricsSource_s3Uri :: Lens.Lens' MetricsSource Prelude.Text
metricsSource_s3Uri = Lens.lens (\MetricsSource' {s3Uri} -> s3Uri) (\s@MetricsSource' {} a -> s {s3Uri = a} :: MetricsSource)

instance Core.FromJSON MetricsSource where
  parseJSON =
    Core.withObject
      "MetricsSource"
      ( \x ->
          MetricsSource'
            Prelude.<$> (x Core..:? "ContentDigest")
            Prelude.<*> (x Core..: "ContentType")
            Prelude.<*> (x Core..: "S3Uri")
      )

instance Prelude.Hashable MetricsSource

instance Prelude.NFData MetricsSource

instance Core.ToJSON MetricsSource where
  toJSON MetricsSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContentDigest" Core..=) Prelude.<$> contentDigest,
            Prelude.Just ("ContentType" Core..= contentType),
            Prelude.Just ("S3Uri" Core..= s3Uri)
          ]
      )
