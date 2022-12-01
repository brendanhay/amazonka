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
-- Module      : Amazonka.MediaLive.Types.Input
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Input where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.InputClass
import Amazonka.MediaLive.Types.InputDestination
import Amazonka.MediaLive.Types.InputDeviceSettings
import Amazonka.MediaLive.Types.InputSource
import Amazonka.MediaLive.Types.InputSourceType
import Amazonka.MediaLive.Types.InputState
import Amazonka.MediaLive.Types.InputType
import Amazonka.MediaLive.Types.MediaConnectFlow
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for Input
--
-- /See:/ 'newInput' smart constructor.
data Input = Input'
  { -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of the sources of the input (PULL-type).
    sources :: Prelude.Maybe [InputSource],
    -- | The user-assigned name (This is a mutable value).
    name :: Prelude.Maybe Prelude.Text,
    type' :: Prelude.Maybe InputType,
    -- | The Amazon Resource Name (ARN) of the role this input assumes during and
    -- after creation.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Settings for the input devices.
    inputDevices :: Prelude.Maybe [InputDeviceSettings],
    -- | A list of MediaConnect Flows for this input.
    mediaConnectFlows :: Prelude.Maybe [MediaConnectFlow],
    -- | The Unique ARN of the input (generated, immutable).
    arn :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe InputState,
    -- | The generated ID of the input (unique for user account, immutable).
    id :: Prelude.Maybe Prelude.Text,
    -- | A list of IDs for all the Input Security Groups attached to the input.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | A list of channel IDs that that input is attached to (currently an input
    -- can only be attached to one channel).
    attachedChannels :: Prelude.Maybe [Prelude.Text],
    -- | Certain pull input sources can be dynamic, meaning that they can have
    -- their URL\'s dynamically changes during input switch actions. Presently,
    -- this functionality only works with MP4_FILE and TS_FILE inputs.
    inputSourceType :: Prelude.Maybe InputSourceType,
    -- | A list of the destinations of the input (PUSH-type).
    destinations :: Prelude.Maybe [InputDestination],
    -- | A list of IDs for all Inputs which are partners of this one.
    inputPartnerIds :: Prelude.Maybe [Prelude.Text],
    -- | STANDARD - MediaLive expects two sources to be connected to this input.
    -- If the channel is also STANDARD, both sources will be ingested. If the
    -- channel is SINGLE_PIPELINE, only the first source will be ingested; the
    -- second source will always be ignored, even if the first source fails.
    -- SINGLE_PIPELINE - You can connect only one source to this input. If the
    -- ChannelClass is also SINGLE_PIPELINE, this value is valid. If the
    -- ChannelClass is STANDARD, this value is not valid because the channel
    -- requires two sources in the input.
    inputClass :: Prelude.Maybe InputClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Input' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'input_tags' - A collection of key-value pairs.
--
-- 'sources', 'input_sources' - A list of the sources of the input (PULL-type).
--
-- 'name', 'input_name' - The user-assigned name (This is a mutable value).
--
-- 'type'', 'input_type' - Undocumented member.
--
-- 'roleArn', 'input_roleArn' - The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
--
-- 'inputDevices', 'input_inputDevices' - Settings for the input devices.
--
-- 'mediaConnectFlows', 'input_mediaConnectFlows' - A list of MediaConnect Flows for this input.
--
-- 'arn', 'input_arn' - The Unique ARN of the input (generated, immutable).
--
-- 'state', 'input_state' - Undocumented member.
--
-- 'id', 'input_id' - The generated ID of the input (unique for user account, immutable).
--
-- 'securityGroups', 'input_securityGroups' - A list of IDs for all the Input Security Groups attached to the input.
--
-- 'attachedChannels', 'input_attachedChannels' - A list of channel IDs that that input is attached to (currently an input
-- can only be attached to one channel).
--
-- 'inputSourceType', 'input_inputSourceType' - Certain pull input sources can be dynamic, meaning that they can have
-- their URL\'s dynamically changes during input switch actions. Presently,
-- this functionality only works with MP4_FILE and TS_FILE inputs.
--
-- 'destinations', 'input_destinations' - A list of the destinations of the input (PUSH-type).
--
-- 'inputPartnerIds', 'input_inputPartnerIds' - A list of IDs for all Inputs which are partners of this one.
--
-- 'inputClass', 'input_inputClass' - STANDARD - MediaLive expects two sources to be connected to this input.
-- If the channel is also STANDARD, both sources will be ingested. If the
-- channel is SINGLE_PIPELINE, only the first source will be ingested; the
-- second source will always be ignored, even if the first source fails.
-- SINGLE_PIPELINE - You can connect only one source to this input. If the
-- ChannelClass is also SINGLE_PIPELINE, this value is valid. If the
-- ChannelClass is STANDARD, this value is not valid because the channel
-- requires two sources in the input.
newInput ::
  Input
newInput =
  Input'
    { tags = Prelude.Nothing,
      sources = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      inputDevices = Prelude.Nothing,
      mediaConnectFlows = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      id = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      attachedChannels = Prelude.Nothing,
      inputSourceType = Prelude.Nothing,
      destinations = Prelude.Nothing,
      inputPartnerIds = Prelude.Nothing,
      inputClass = Prelude.Nothing
    }

-- | A collection of key-value pairs.
input_tags :: Lens.Lens' Input (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
input_tags = Lens.lens (\Input' {tags} -> tags) (\s@Input' {} a -> s {tags = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | A list of the sources of the input (PULL-type).
input_sources :: Lens.Lens' Input (Prelude.Maybe [InputSource])
input_sources = Lens.lens (\Input' {sources} -> sources) (\s@Input' {} a -> s {sources = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | The user-assigned name (This is a mutable value).
input_name :: Lens.Lens' Input (Prelude.Maybe Prelude.Text)
input_name = Lens.lens (\Input' {name} -> name) (\s@Input' {} a -> s {name = a} :: Input)

-- | Undocumented member.
input_type :: Lens.Lens' Input (Prelude.Maybe InputType)
input_type = Lens.lens (\Input' {type'} -> type') (\s@Input' {} a -> s {type' = a} :: Input)

-- | The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
input_roleArn :: Lens.Lens' Input (Prelude.Maybe Prelude.Text)
input_roleArn = Lens.lens (\Input' {roleArn} -> roleArn) (\s@Input' {} a -> s {roleArn = a} :: Input)

-- | Settings for the input devices.
input_inputDevices :: Lens.Lens' Input (Prelude.Maybe [InputDeviceSettings])
input_inputDevices = Lens.lens (\Input' {inputDevices} -> inputDevices) (\s@Input' {} a -> s {inputDevices = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | A list of MediaConnect Flows for this input.
input_mediaConnectFlows :: Lens.Lens' Input (Prelude.Maybe [MediaConnectFlow])
input_mediaConnectFlows = Lens.lens (\Input' {mediaConnectFlows} -> mediaConnectFlows) (\s@Input' {} a -> s {mediaConnectFlows = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | The Unique ARN of the input (generated, immutable).
input_arn :: Lens.Lens' Input (Prelude.Maybe Prelude.Text)
input_arn = Lens.lens (\Input' {arn} -> arn) (\s@Input' {} a -> s {arn = a} :: Input)

-- | Undocumented member.
input_state :: Lens.Lens' Input (Prelude.Maybe InputState)
input_state = Lens.lens (\Input' {state} -> state) (\s@Input' {} a -> s {state = a} :: Input)

-- | The generated ID of the input (unique for user account, immutable).
input_id :: Lens.Lens' Input (Prelude.Maybe Prelude.Text)
input_id = Lens.lens (\Input' {id} -> id) (\s@Input' {} a -> s {id = a} :: Input)

-- | A list of IDs for all the Input Security Groups attached to the input.
input_securityGroups :: Lens.Lens' Input (Prelude.Maybe [Prelude.Text])
input_securityGroups = Lens.lens (\Input' {securityGroups} -> securityGroups) (\s@Input' {} a -> s {securityGroups = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | A list of channel IDs that that input is attached to (currently an input
-- can only be attached to one channel).
input_attachedChannels :: Lens.Lens' Input (Prelude.Maybe [Prelude.Text])
input_attachedChannels = Lens.lens (\Input' {attachedChannels} -> attachedChannels) (\s@Input' {} a -> s {attachedChannels = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | Certain pull input sources can be dynamic, meaning that they can have
-- their URL\'s dynamically changes during input switch actions. Presently,
-- this functionality only works with MP4_FILE and TS_FILE inputs.
input_inputSourceType :: Lens.Lens' Input (Prelude.Maybe InputSourceType)
input_inputSourceType = Lens.lens (\Input' {inputSourceType} -> inputSourceType) (\s@Input' {} a -> s {inputSourceType = a} :: Input)

-- | A list of the destinations of the input (PUSH-type).
input_destinations :: Lens.Lens' Input (Prelude.Maybe [InputDestination])
input_destinations = Lens.lens (\Input' {destinations} -> destinations) (\s@Input' {} a -> s {destinations = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | A list of IDs for all Inputs which are partners of this one.
input_inputPartnerIds :: Lens.Lens' Input (Prelude.Maybe [Prelude.Text])
input_inputPartnerIds = Lens.lens (\Input' {inputPartnerIds} -> inputPartnerIds) (\s@Input' {} a -> s {inputPartnerIds = a} :: Input) Prelude.. Lens.mapping Lens.coerced

-- | STANDARD - MediaLive expects two sources to be connected to this input.
-- If the channel is also STANDARD, both sources will be ingested. If the
-- channel is SINGLE_PIPELINE, only the first source will be ingested; the
-- second source will always be ignored, even if the first source fails.
-- SINGLE_PIPELINE - You can connect only one source to this input. If the
-- ChannelClass is also SINGLE_PIPELINE, this value is valid. If the
-- ChannelClass is STANDARD, this value is not valid because the channel
-- requires two sources in the input.
input_inputClass :: Lens.Lens' Input (Prelude.Maybe InputClass)
input_inputClass = Lens.lens (\Input' {inputClass} -> inputClass) (\s@Input' {} a -> s {inputClass = a} :: Input)

instance Core.FromJSON Input where
  parseJSON =
    Core.withObject
      "Input"
      ( \x ->
          Input'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "sources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "inputDevices" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "mediaConnectFlows"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "securityGroups" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "attachedChannels"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "inputSourceType")
            Prelude.<*> (x Core..:? "destinations" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "inputPartnerIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "inputClass")
      )

instance Prelude.Hashable Input where
  hashWithSalt _salt Input' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` inputDevices
      `Prelude.hashWithSalt` mediaConnectFlows
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` attachedChannels
      `Prelude.hashWithSalt` inputSourceType
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` inputPartnerIds
      `Prelude.hashWithSalt` inputClass

instance Prelude.NFData Input where
  rnf Input' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf inputDevices
      `Prelude.seq` Prelude.rnf mediaConnectFlows
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf attachedChannels
      `Prelude.seq` Prelude.rnf inputSourceType
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf inputPartnerIds
      `Prelude.seq` Prelude.rnf inputClass
