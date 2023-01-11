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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.DataChannelConcatenationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.DataChannelConcatenationConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The content configuration object\'s data channel.
--
-- /See:/ 'newDataChannelConcatenationConfiguration' smart constructor.
data DataChannelConcatenationConfiguration = DataChannelConcatenationConfiguration'
  { -- | Enables or disables the configuration object.
    state :: ArtifactsConcatenationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataChannelConcatenationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'dataChannelConcatenationConfiguration_state' - Enables or disables the configuration object.
newDataChannelConcatenationConfiguration ::
  -- | 'state'
  ArtifactsConcatenationState ->
  DataChannelConcatenationConfiguration
newDataChannelConcatenationConfiguration pState_ =
  DataChannelConcatenationConfiguration'
    { state =
        pState_
    }

-- | Enables or disables the configuration object.
dataChannelConcatenationConfiguration_state :: Lens.Lens' DataChannelConcatenationConfiguration ArtifactsConcatenationState
dataChannelConcatenationConfiguration_state = Lens.lens (\DataChannelConcatenationConfiguration' {state} -> state) (\s@DataChannelConcatenationConfiguration' {} a -> s {state = a} :: DataChannelConcatenationConfiguration)

instance
  Data.FromJSON
    DataChannelConcatenationConfiguration
  where
  parseJSON =
    Data.withObject
      "DataChannelConcatenationConfiguration"
      ( \x ->
          DataChannelConcatenationConfiguration'
            Prelude.<$> (x Data..: "State")
      )

instance
  Prelude.Hashable
    DataChannelConcatenationConfiguration
  where
  hashWithSalt
    _salt
    DataChannelConcatenationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    DataChannelConcatenationConfiguration
  where
  rnf DataChannelConcatenationConfiguration' {..} =
    Prelude.rnf state

instance
  Data.ToJSON
    DataChannelConcatenationConfiguration
  where
  toJSON DataChannelConcatenationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("State" Data..= state)]
      )
