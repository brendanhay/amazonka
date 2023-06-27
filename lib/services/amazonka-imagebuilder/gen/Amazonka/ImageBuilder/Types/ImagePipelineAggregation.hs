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
-- Module      : Amazonka.ImageBuilder.Types.ImagePipelineAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImagePipelineAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | Contains vulnerability counts for a specific image pipeline.
--
-- /See:/ 'newImagePipelineAggregation' smart constructor.
data ImagePipelineAggregation = ImagePipelineAggregation'
  { -- | The Amazon Resource Name (ARN) that identifies the image pipeline for
    -- this aggregation.
    imagePipelineArn :: Prelude.Maybe Prelude.Text,
    -- | Counts by severity level for medium severity and higher level findings,
    -- plus a total for all of the findings for the specified image pipeline.
    severityCounts :: Prelude.Maybe SeverityCounts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImagePipelineAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imagePipelineArn', 'imagePipelineAggregation_imagePipelineArn' - The Amazon Resource Name (ARN) that identifies the image pipeline for
-- this aggregation.
--
-- 'severityCounts', 'imagePipelineAggregation_severityCounts' - Counts by severity level for medium severity and higher level findings,
-- plus a total for all of the findings for the specified image pipeline.
newImagePipelineAggregation ::
  ImagePipelineAggregation
newImagePipelineAggregation =
  ImagePipelineAggregation'
    { imagePipelineArn =
        Prelude.Nothing,
      severityCounts = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the image pipeline for
-- this aggregation.
imagePipelineAggregation_imagePipelineArn :: Lens.Lens' ImagePipelineAggregation (Prelude.Maybe Prelude.Text)
imagePipelineAggregation_imagePipelineArn = Lens.lens (\ImagePipelineAggregation' {imagePipelineArn} -> imagePipelineArn) (\s@ImagePipelineAggregation' {} a -> s {imagePipelineArn = a} :: ImagePipelineAggregation)

-- | Counts by severity level for medium severity and higher level findings,
-- plus a total for all of the findings for the specified image pipeline.
imagePipelineAggregation_severityCounts :: Lens.Lens' ImagePipelineAggregation (Prelude.Maybe SeverityCounts)
imagePipelineAggregation_severityCounts = Lens.lens (\ImagePipelineAggregation' {severityCounts} -> severityCounts) (\s@ImagePipelineAggregation' {} a -> s {severityCounts = a} :: ImagePipelineAggregation)

instance Data.FromJSON ImagePipelineAggregation where
  parseJSON =
    Data.withObject
      "ImagePipelineAggregation"
      ( \x ->
          ImagePipelineAggregation'
            Prelude.<$> (x Data..:? "imagePipelineArn")
            Prelude.<*> (x Data..:? "severityCounts")
      )

instance Prelude.Hashable ImagePipelineAggregation where
  hashWithSalt _salt ImagePipelineAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` imagePipelineArn
      `Prelude.hashWithSalt` severityCounts

instance Prelude.NFData ImagePipelineAggregation where
  rnf ImagePipelineAggregation' {..} =
    Prelude.rnf imagePipelineArn
      `Prelude.seq` Prelude.rnf severityCounts
