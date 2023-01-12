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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.SourceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.SourceConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.SelectedVideoStreams
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Source configuration for a specified media pipeline.
--
-- /See:/ 'newSourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
  { -- | The selected video streams for a specified media pipeline. The number of
    -- video streams can\'t exceed 25.
    selectedVideoStreams :: Prelude.Maybe SelectedVideoStreams
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectedVideoStreams', 'sourceConfiguration_selectedVideoStreams' - The selected video streams for a specified media pipeline. The number of
-- video streams can\'t exceed 25.
newSourceConfiguration ::
  SourceConfiguration
newSourceConfiguration =
  SourceConfiguration'
    { selectedVideoStreams =
        Prelude.Nothing
    }

-- | The selected video streams for a specified media pipeline. The number of
-- video streams can\'t exceed 25.
sourceConfiguration_selectedVideoStreams :: Lens.Lens' SourceConfiguration (Prelude.Maybe SelectedVideoStreams)
sourceConfiguration_selectedVideoStreams = Lens.lens (\SourceConfiguration' {selectedVideoStreams} -> selectedVideoStreams) (\s@SourceConfiguration' {} a -> s {selectedVideoStreams = a} :: SourceConfiguration)

instance Data.FromJSON SourceConfiguration where
  parseJSON =
    Data.withObject
      "SourceConfiguration"
      ( \x ->
          SourceConfiguration'
            Prelude.<$> (x Data..:? "SelectedVideoStreams")
      )

instance Prelude.Hashable SourceConfiguration where
  hashWithSalt _salt SourceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` selectedVideoStreams

instance Prelude.NFData SourceConfiguration where
  rnf SourceConfiguration' {..} =
    Prelude.rnf selectedVideoStreams

instance Data.ToJSON SourceConfiguration where
  toJSON SourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SelectedVideoStreams" Data..=)
              Prelude.<$> selectedVideoStreams
          ]
      )
