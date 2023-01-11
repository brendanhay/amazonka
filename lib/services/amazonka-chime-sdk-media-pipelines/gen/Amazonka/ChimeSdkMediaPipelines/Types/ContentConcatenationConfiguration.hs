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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ContentConcatenationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ContentConcatenationConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The composited content configuration object for a specified media
-- pipeline.
--
-- /See:/ 'newContentConcatenationConfiguration' smart constructor.
data ContentConcatenationConfiguration = ContentConcatenationConfiguration'
  { -- | Enables or disables the configuration object.
    state :: ArtifactsConcatenationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContentConcatenationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'contentConcatenationConfiguration_state' - Enables or disables the configuration object.
newContentConcatenationConfiguration ::
  -- | 'state'
  ArtifactsConcatenationState ->
  ContentConcatenationConfiguration
newContentConcatenationConfiguration pState_ =
  ContentConcatenationConfiguration' {state = pState_}

-- | Enables or disables the configuration object.
contentConcatenationConfiguration_state :: Lens.Lens' ContentConcatenationConfiguration ArtifactsConcatenationState
contentConcatenationConfiguration_state = Lens.lens (\ContentConcatenationConfiguration' {state} -> state) (\s@ContentConcatenationConfiguration' {} a -> s {state = a} :: ContentConcatenationConfiguration)

instance
  Data.FromJSON
    ContentConcatenationConfiguration
  where
  parseJSON =
    Data.withObject
      "ContentConcatenationConfiguration"
      ( \x ->
          ContentConcatenationConfiguration'
            Prelude.<$> (x Data..: "State")
      )

instance
  Prelude.Hashable
    ContentConcatenationConfiguration
  where
  hashWithSalt
    _salt
    ContentConcatenationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    ContentConcatenationConfiguration
  where
  rnf ContentConcatenationConfiguration' {..} =
    Prelude.rnf state

instance
  Data.ToJSON
    ContentConcatenationConfiguration
  where
  toJSON ContentConcatenationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("State" Data..= state)]
      )
