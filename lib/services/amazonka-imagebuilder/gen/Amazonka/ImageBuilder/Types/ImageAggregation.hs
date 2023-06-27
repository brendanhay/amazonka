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
-- Module      : Amazonka.ImageBuilder.Types.ImageAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | Contains vulnerability counts for a specific image.
--
-- /See:/ 'newImageAggregation' smart constructor.
data ImageAggregation = ImageAggregation'
  { -- | The Amazon Resource Name (ARN) that identifies the image for this
    -- aggregation.
    imageBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | Counts by severity level for medium severity and higher level findings,
    -- plus a total for all of the findings for the specified image.
    severityCounts :: Prelude.Maybe SeverityCounts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuildVersionArn', 'imageAggregation_imageBuildVersionArn' - The Amazon Resource Name (ARN) that identifies the image for this
-- aggregation.
--
-- 'severityCounts', 'imageAggregation_severityCounts' - Counts by severity level for medium severity and higher level findings,
-- plus a total for all of the findings for the specified image.
newImageAggregation ::
  ImageAggregation
newImageAggregation =
  ImageAggregation'
    { imageBuildVersionArn =
        Prelude.Nothing,
      severityCounts = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the image for this
-- aggregation.
imageAggregation_imageBuildVersionArn :: Lens.Lens' ImageAggregation (Prelude.Maybe Prelude.Text)
imageAggregation_imageBuildVersionArn = Lens.lens (\ImageAggregation' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@ImageAggregation' {} a -> s {imageBuildVersionArn = a} :: ImageAggregation)

-- | Counts by severity level for medium severity and higher level findings,
-- plus a total for all of the findings for the specified image.
imageAggregation_severityCounts :: Lens.Lens' ImageAggregation (Prelude.Maybe SeverityCounts)
imageAggregation_severityCounts = Lens.lens (\ImageAggregation' {severityCounts} -> severityCounts) (\s@ImageAggregation' {} a -> s {severityCounts = a} :: ImageAggregation)

instance Data.FromJSON ImageAggregation where
  parseJSON =
    Data.withObject
      "ImageAggregation"
      ( \x ->
          ImageAggregation'
            Prelude.<$> (x Data..:? "imageBuildVersionArn")
            Prelude.<*> (x Data..:? "severityCounts")
      )

instance Prelude.Hashable ImageAggregation where
  hashWithSalt _salt ImageAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` imageBuildVersionArn
      `Prelude.hashWithSalt` severityCounts

instance Prelude.NFData ImageAggregation where
  rnf ImageAggregation' {..} =
    Prelude.rnf imageBuildVersionArn
      `Prelude.seq` Prelude.rnf severityCounts
