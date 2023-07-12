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
-- Module      : Amazonka.IVS.Types.RecordingConfigurationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.RecordingConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.DestinationConfiguration
import Amazonka.IVS.Types.RecordingConfigurationState
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a RecordingConfiguration.
--
-- /See:/ 'newRecordingConfigurationSummary' smart constructor.
data RecordingConfigurationSummary = RecordingConfigurationSummary'
  { -- | Recording-configuration name. The value does not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- for more information, including restrictions that apply to tags and
    -- \"Tag naming limits and requirements\"; Amazon IVS has no
    -- service-specific constraints beyond what is documented there.
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
-- Create a value of 'RecordingConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'recordingConfigurationSummary_name' - Recording-configuration name. The value does not need to be unique.
--
-- 'tags', 'recordingConfigurationSummary_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
--
-- 'arn', 'recordingConfigurationSummary_arn' - Recording-configuration ARN.
--
-- 'destinationConfiguration', 'recordingConfigurationSummary_destinationConfiguration' - A complex type that contains information about where recorded video will
-- be stored.
--
-- 'state', 'recordingConfigurationSummary_state' - Indicates the current state of the recording configuration. When the
-- state is @ACTIVE@, the configuration is ready for recording a channel
-- stream.
newRecordingConfigurationSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'destinationConfiguration'
  DestinationConfiguration ->
  -- | 'state'
  RecordingConfigurationState ->
  RecordingConfigurationSummary
newRecordingConfigurationSummary
  pArn_
  pDestinationConfiguration_
  pState_ =
    RecordingConfigurationSummary'
      { name =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        arn = pArn_,
        destinationConfiguration =
          pDestinationConfiguration_,
        state = pState_
      }

-- | Recording-configuration name. The value does not need to be unique.
recordingConfigurationSummary_name :: Lens.Lens' RecordingConfigurationSummary (Prelude.Maybe Prelude.Text)
recordingConfigurationSummary_name = Lens.lens (\RecordingConfigurationSummary' {name} -> name) (\s@RecordingConfigurationSummary' {} a -> s {name = a} :: RecordingConfigurationSummary)

-- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
recordingConfigurationSummary_tags :: Lens.Lens' RecordingConfigurationSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recordingConfigurationSummary_tags = Lens.lens (\RecordingConfigurationSummary' {tags} -> tags) (\s@RecordingConfigurationSummary' {} a -> s {tags = a} :: RecordingConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

-- | Recording-configuration ARN.
recordingConfigurationSummary_arn :: Lens.Lens' RecordingConfigurationSummary Prelude.Text
recordingConfigurationSummary_arn = Lens.lens (\RecordingConfigurationSummary' {arn} -> arn) (\s@RecordingConfigurationSummary' {} a -> s {arn = a} :: RecordingConfigurationSummary)

-- | A complex type that contains information about where recorded video will
-- be stored.
recordingConfigurationSummary_destinationConfiguration :: Lens.Lens' RecordingConfigurationSummary DestinationConfiguration
recordingConfigurationSummary_destinationConfiguration = Lens.lens (\RecordingConfigurationSummary' {destinationConfiguration} -> destinationConfiguration) (\s@RecordingConfigurationSummary' {} a -> s {destinationConfiguration = a} :: RecordingConfigurationSummary)

-- | Indicates the current state of the recording configuration. When the
-- state is @ACTIVE@, the configuration is ready for recording a channel
-- stream.
recordingConfigurationSummary_state :: Lens.Lens' RecordingConfigurationSummary RecordingConfigurationState
recordingConfigurationSummary_state = Lens.lens (\RecordingConfigurationSummary' {state} -> state) (\s@RecordingConfigurationSummary' {} a -> s {state = a} :: RecordingConfigurationSummary)

instance Data.FromJSON RecordingConfigurationSummary where
  parseJSON =
    Data.withObject
      "RecordingConfigurationSummary"
      ( \x ->
          RecordingConfigurationSummary'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "destinationConfiguration")
            Prelude.<*> (x Data..: "state")
      )

instance
  Prelude.Hashable
    RecordingConfigurationSummary
  where
  hashWithSalt _salt RecordingConfigurationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` destinationConfiguration
      `Prelude.hashWithSalt` state

instance Prelude.NFData RecordingConfigurationSummary where
  rnf RecordingConfigurationSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf destinationConfiguration
      `Prelude.seq` Prelude.rnf state
