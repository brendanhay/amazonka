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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSource where

import Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSourceType
import Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipelineSourceConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The source type and media pipeline configuration settings in a
-- configuration object.
--
-- /See:/ 'newConcatenationSource' smart constructor.
data ConcatenationSource = ConcatenationSource'
  { -- | The type of concatenation source in a configuration object.
    type' :: ConcatenationSourceType,
    -- | The concatenation settings for the media pipeline in a configuration
    -- object.
    mediaCapturePipelineSourceConfiguration :: MediaCapturePipelineSourceConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConcatenationSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'concatenationSource_type' - The type of concatenation source in a configuration object.
--
-- 'mediaCapturePipelineSourceConfiguration', 'concatenationSource_mediaCapturePipelineSourceConfiguration' - The concatenation settings for the media pipeline in a configuration
-- object.
newConcatenationSource ::
  -- | 'type''
  ConcatenationSourceType ->
  -- | 'mediaCapturePipelineSourceConfiguration'
  MediaCapturePipelineSourceConfiguration ->
  ConcatenationSource
newConcatenationSource
  pType_
  pMediaCapturePipelineSourceConfiguration_ =
    ConcatenationSource'
      { type' = pType_,
        mediaCapturePipelineSourceConfiguration =
          pMediaCapturePipelineSourceConfiguration_
      }

-- | The type of concatenation source in a configuration object.
concatenationSource_type :: Lens.Lens' ConcatenationSource ConcatenationSourceType
concatenationSource_type = Lens.lens (\ConcatenationSource' {type'} -> type') (\s@ConcatenationSource' {} a -> s {type' = a} :: ConcatenationSource)

-- | The concatenation settings for the media pipeline in a configuration
-- object.
concatenationSource_mediaCapturePipelineSourceConfiguration :: Lens.Lens' ConcatenationSource MediaCapturePipelineSourceConfiguration
concatenationSource_mediaCapturePipelineSourceConfiguration = Lens.lens (\ConcatenationSource' {mediaCapturePipelineSourceConfiguration} -> mediaCapturePipelineSourceConfiguration) (\s@ConcatenationSource' {} a -> s {mediaCapturePipelineSourceConfiguration = a} :: ConcatenationSource)

instance Core.FromJSON ConcatenationSource where
  parseJSON =
    Core.withObject
      "ConcatenationSource"
      ( \x ->
          ConcatenationSource'
            Prelude.<$> (x Core..: "Type")
            Prelude.<*> ( x
                            Core..: "MediaCapturePipelineSourceConfiguration"
                        )
      )

instance Prelude.Hashable ConcatenationSource where
  hashWithSalt _salt ConcatenationSource' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` mediaCapturePipelineSourceConfiguration

instance Prelude.NFData ConcatenationSource where
  rnf ConcatenationSource' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf mediaCapturePipelineSourceConfiguration

instance Core.ToJSON ConcatenationSource where
  toJSON ConcatenationSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Core..= type'),
            Prelude.Just
              ( "MediaCapturePipelineSourceConfiguration"
                  Core..= mediaCapturePipelineSourceConfiguration
              )
          ]
      )
