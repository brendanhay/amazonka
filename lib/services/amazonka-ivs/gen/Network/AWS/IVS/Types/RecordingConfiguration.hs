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
-- Module      : Network.AWS.IVS.Types.RecordingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IVS.Types.RecordingConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.IVS.Types.DestinationConfiguration
import Network.AWS.IVS.Types.RecordingConfigurationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a configuration to record a channel stream.
--
-- /See:/ 'newRecordingConfiguration' smart constructor.
data RecordingConfiguration = RecordingConfiguration'
  { -- | Recording-configuration name. The value does not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Array of 1-50 maps, each of the form @string:string (key:value)@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Recording-configuration ARN.
    arn :: Prelude.Text,
    -- | A complex type that contains information about where recorded video will
    -- be stored.
    destinationConfiguration :: DestinationConfiguration,
    -- | Indicates the current state of the recording configuration. When the
    -- state is @ACTIVE@, the configuration is ready for recording a channel
    -- stream.
    state :: RecordingConfigurationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'recordingConfiguration_name' - Recording-configuration name. The value does not need to be unique.
--
-- 'tags', 'recordingConfiguration_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@.
--
-- 'arn', 'recordingConfiguration_arn' - Recording-configuration ARN.
--
-- 'destinationConfiguration', 'recordingConfiguration_destinationConfiguration' - A complex type that contains information about where recorded video will
-- be stored.
--
-- 'state', 'recordingConfiguration_state' - Indicates the current state of the recording configuration. When the
-- state is @ACTIVE@, the configuration is ready for recording a channel
-- stream.
newRecordingConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'destinationConfiguration'
  DestinationConfiguration ->
  -- | 'state'
  RecordingConfigurationState ->
  RecordingConfiguration
newRecordingConfiguration
  pArn_
  pDestinationConfiguration_
  pState_ =
    RecordingConfiguration'
      { name = Prelude.Nothing,
        tags = Prelude.Nothing,
        arn = pArn_,
        destinationConfiguration =
          pDestinationConfiguration_,
        state = pState_
      }

-- | Recording-configuration name. The value does not need to be unique.
recordingConfiguration_name :: Lens.Lens' RecordingConfiguration (Prelude.Maybe Prelude.Text)
recordingConfiguration_name = Lens.lens (\RecordingConfiguration' {name} -> name) (\s@RecordingConfiguration' {} a -> s {name = a} :: RecordingConfiguration)

-- | Array of 1-50 maps, each of the form @string:string (key:value)@.
recordingConfiguration_tags :: Lens.Lens' RecordingConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recordingConfiguration_tags = Lens.lens (\RecordingConfiguration' {tags} -> tags) (\s@RecordingConfiguration' {} a -> s {tags = a} :: RecordingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Recording-configuration ARN.
recordingConfiguration_arn :: Lens.Lens' RecordingConfiguration Prelude.Text
recordingConfiguration_arn = Lens.lens (\RecordingConfiguration' {arn} -> arn) (\s@RecordingConfiguration' {} a -> s {arn = a} :: RecordingConfiguration)

-- | A complex type that contains information about where recorded video will
-- be stored.
recordingConfiguration_destinationConfiguration :: Lens.Lens' RecordingConfiguration DestinationConfiguration
recordingConfiguration_destinationConfiguration = Lens.lens (\RecordingConfiguration' {destinationConfiguration} -> destinationConfiguration) (\s@RecordingConfiguration' {} a -> s {destinationConfiguration = a} :: RecordingConfiguration)

-- | Indicates the current state of the recording configuration. When the
-- state is @ACTIVE@, the configuration is ready for recording a channel
-- stream.
recordingConfiguration_state :: Lens.Lens' RecordingConfiguration RecordingConfigurationState
recordingConfiguration_state = Lens.lens (\RecordingConfiguration' {state} -> state) (\s@RecordingConfiguration' {} a -> s {state = a} :: RecordingConfiguration)

instance Core.FromJSON RecordingConfiguration where
  parseJSON =
    Core.withObject
      "RecordingConfiguration"
      ( \x ->
          RecordingConfiguration'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "destinationConfiguration")
            Prelude.<*> (x Core..: "state")
      )

instance Prelude.Hashable RecordingConfiguration

instance Prelude.NFData RecordingConfiguration
