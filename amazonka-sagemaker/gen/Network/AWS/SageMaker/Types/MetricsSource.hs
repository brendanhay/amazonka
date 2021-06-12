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
-- Module      : Network.AWS.SageMaker.Types.MetricsSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MetricsSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- |
--
-- /See:/ 'newMetricsSource' smart constructor.
data MetricsSource = MetricsSource'
  { contentDigest :: Core.Maybe Core.Text,
    contentType :: Core.Text,
    s3Uri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 's3Uri'
  Core.Text ->
  MetricsSource
newMetricsSource pContentType_ pS3Uri_ =
  MetricsSource'
    { contentDigest = Core.Nothing,
      contentType = pContentType_,
      s3Uri = pS3Uri_
    }

-- |
metricsSource_contentDigest :: Lens.Lens' MetricsSource (Core.Maybe Core.Text)
metricsSource_contentDigest = Lens.lens (\MetricsSource' {contentDigest} -> contentDigest) (\s@MetricsSource' {} a -> s {contentDigest = a} :: MetricsSource)

-- |
metricsSource_contentType :: Lens.Lens' MetricsSource Core.Text
metricsSource_contentType = Lens.lens (\MetricsSource' {contentType} -> contentType) (\s@MetricsSource' {} a -> s {contentType = a} :: MetricsSource)

-- |
metricsSource_s3Uri :: Lens.Lens' MetricsSource Core.Text
metricsSource_s3Uri = Lens.lens (\MetricsSource' {s3Uri} -> s3Uri) (\s@MetricsSource' {} a -> s {s3Uri = a} :: MetricsSource)

instance Core.FromJSON MetricsSource where
  parseJSON =
    Core.withObject
      "MetricsSource"
      ( \x ->
          MetricsSource'
            Core.<$> (x Core..:? "ContentDigest")
            Core.<*> (x Core..: "ContentType")
            Core.<*> (x Core..: "S3Uri")
      )

instance Core.Hashable MetricsSource

instance Core.NFData MetricsSource

instance Core.ToJSON MetricsSource where
  toJSON MetricsSource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContentDigest" Core..=) Core.<$> contentDigest,
            Core.Just ("ContentType" Core..= contentType),
            Core.Just ("S3Uri" Core..= s3Uri)
          ]
      )
