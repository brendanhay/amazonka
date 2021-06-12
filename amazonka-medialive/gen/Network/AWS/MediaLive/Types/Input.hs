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
-- Module      : Network.AWS.MediaLive.Types.Input
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Input where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputClass
import Network.AWS.MediaLive.Types.InputDestination
import Network.AWS.MediaLive.Types.InputDeviceSettings
import Network.AWS.MediaLive.Types.InputSource
import Network.AWS.MediaLive.Types.InputSourceType
import Network.AWS.MediaLive.Types.InputState
import Network.AWS.MediaLive.Types.InputType
import Network.AWS.MediaLive.Types.MediaConnectFlow

-- | Placeholder documentation for Input
--
-- /See:/ 'newInput' smart constructor.
data Input = Input'
  { -- | The Amazon Resource Name (ARN) of the role this input assumes during and
    -- after creation.
    roleArn :: Core.Maybe Core.Text,
    -- | A list of the sources of the input (PULL-type).
    sources :: Core.Maybe [InputSource],
    -- | A list of IDs for all Inputs which are partners of this one.
    inputPartnerIds :: Core.Maybe [Core.Text],
    -- | Certain pull input sources can be dynamic, meaning that they can have
    -- their URL\'s dynamically changes during input switch actions. Presently,
    -- this functionality only works with MP4_FILE inputs.
    inputSourceType :: Core.Maybe InputSourceType,
    -- | A list of MediaConnect Flows for this input.
    mediaConnectFlows :: Core.Maybe [MediaConnectFlow],
    -- | The Unique ARN of the input (generated, immutable).
    arn :: Core.Maybe Core.Text,
    -- | The generated ID of the input (unique for user account, immutable).
    id :: Core.Maybe Core.Text,
    -- | A list of IDs for all the Input Security Groups attached to the input.
    securityGroups :: Core.Maybe [Core.Text],
    -- | A list of the destinations of the input (PUSH-type).
    destinations :: Core.Maybe [InputDestination],
    state :: Core.Maybe InputState,
    -- | The user-assigned name (This is a mutable value).
    name :: Core.Maybe Core.Text,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    type' :: Core.Maybe InputType,
    -- | STANDARD - MediaLive expects two sources to be connected to this input.
    -- If the channel is also STANDARD, both sources will be ingested. If the
    -- channel is SINGLE_PIPELINE, only the first source will be ingested; the
    -- second source will always be ignored, even if the first source fails.
    -- SINGLE_PIPELINE - You can connect only one source to this input. If the
    -- ChannelClass is also SINGLE_PIPELINE, this value is valid. If the
    -- ChannelClass is STANDARD, this value is not valid because the channel
    -- requires two sources in the input.
    inputClass :: Core.Maybe InputClass,
    -- | Settings for the input devices.
    inputDevices :: Core.Maybe [InputDeviceSettings],
    -- | A list of channel IDs that that input is attached to (currently an input
    -- can only be attached to one channel).
    attachedChannels :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Input' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'input_roleArn' - The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
--
-- 'sources', 'input_sources' - A list of the sources of the input (PULL-type).
--
-- 'inputPartnerIds', 'input_inputPartnerIds' - A list of IDs for all Inputs which are partners of this one.
--
-- 'inputSourceType', 'input_inputSourceType' - Certain pull input sources can be dynamic, meaning that they can have
-- their URL\'s dynamically changes during input switch actions. Presently,
-- this functionality only works with MP4_FILE inputs.
--
-- 'mediaConnectFlows', 'input_mediaConnectFlows' - A list of MediaConnect Flows for this input.
--
-- 'arn', 'input_arn' - The Unique ARN of the input (generated, immutable).
--
-- 'id', 'input_id' - The generated ID of the input (unique for user account, immutable).
--
-- 'securityGroups', 'input_securityGroups' - A list of IDs for all the Input Security Groups attached to the input.
--
-- 'destinations', 'input_destinations' - A list of the destinations of the input (PUSH-type).
--
-- 'state', 'input_state' - Undocumented member.
--
-- 'name', 'input_name' - The user-assigned name (This is a mutable value).
--
-- 'tags', 'input_tags' - A collection of key-value pairs.
--
-- 'type'', 'input_type' - Undocumented member.
--
-- 'inputClass', 'input_inputClass' - STANDARD - MediaLive expects two sources to be connected to this input.
-- If the channel is also STANDARD, both sources will be ingested. If the
-- channel is SINGLE_PIPELINE, only the first source will be ingested; the
-- second source will always be ignored, even if the first source fails.
-- SINGLE_PIPELINE - You can connect only one source to this input. If the
-- ChannelClass is also SINGLE_PIPELINE, this value is valid. If the
-- ChannelClass is STANDARD, this value is not valid because the channel
-- requires two sources in the input.
--
-- 'inputDevices', 'input_inputDevices' - Settings for the input devices.
--
-- 'attachedChannels', 'input_attachedChannels' - A list of channel IDs that that input is attached to (currently an input
-- can only be attached to one channel).
newInput ::
  Input
newInput =
  Input'
    { roleArn = Core.Nothing,
      sources = Core.Nothing,
      inputPartnerIds = Core.Nothing,
      inputSourceType = Core.Nothing,
      mediaConnectFlows = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      securityGroups = Core.Nothing,
      destinations = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      type' = Core.Nothing,
      inputClass = Core.Nothing,
      inputDevices = Core.Nothing,
      attachedChannels = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
input_roleArn :: Lens.Lens' Input (Core.Maybe Core.Text)
input_roleArn = Lens.lens (\Input' {roleArn} -> roleArn) (\s@Input' {} a -> s {roleArn = a} :: Input)

-- | A list of the sources of the input (PULL-type).
input_sources :: Lens.Lens' Input (Core.Maybe [InputSource])
input_sources = Lens.lens (\Input' {sources} -> sources) (\s@Input' {} a -> s {sources = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | A list of IDs for all Inputs which are partners of this one.
input_inputPartnerIds :: Lens.Lens' Input (Core.Maybe [Core.Text])
input_inputPartnerIds = Lens.lens (\Input' {inputPartnerIds} -> inputPartnerIds) (\s@Input' {} a -> s {inputPartnerIds = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | Certain pull input sources can be dynamic, meaning that they can have
-- their URL\'s dynamically changes during input switch actions. Presently,
-- this functionality only works with MP4_FILE inputs.
input_inputSourceType :: Lens.Lens' Input (Core.Maybe InputSourceType)
input_inputSourceType = Lens.lens (\Input' {inputSourceType} -> inputSourceType) (\s@Input' {} a -> s {inputSourceType = a} :: Input)

-- | A list of MediaConnect Flows for this input.
input_mediaConnectFlows :: Lens.Lens' Input (Core.Maybe [MediaConnectFlow])
input_mediaConnectFlows = Lens.lens (\Input' {mediaConnectFlows} -> mediaConnectFlows) (\s@Input' {} a -> s {mediaConnectFlows = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | The Unique ARN of the input (generated, immutable).
input_arn :: Lens.Lens' Input (Core.Maybe Core.Text)
input_arn = Lens.lens (\Input' {arn} -> arn) (\s@Input' {} a -> s {arn = a} :: Input)

-- | The generated ID of the input (unique for user account, immutable).
input_id :: Lens.Lens' Input (Core.Maybe Core.Text)
input_id = Lens.lens (\Input' {id} -> id) (\s@Input' {} a -> s {id = a} :: Input)

-- | A list of IDs for all the Input Security Groups attached to the input.
input_securityGroups :: Lens.Lens' Input (Core.Maybe [Core.Text])
input_securityGroups = Lens.lens (\Input' {securityGroups} -> securityGroups) (\s@Input' {} a -> s {securityGroups = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | A list of the destinations of the input (PUSH-type).
input_destinations :: Lens.Lens' Input (Core.Maybe [InputDestination])
input_destinations = Lens.lens (\Input' {destinations} -> destinations) (\s@Input' {} a -> s {destinations = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
input_state :: Lens.Lens' Input (Core.Maybe InputState)
input_state = Lens.lens (\Input' {state} -> state) (\s@Input' {} a -> s {state = a} :: Input)

-- | The user-assigned name (This is a mutable value).
input_name :: Lens.Lens' Input (Core.Maybe Core.Text)
input_name = Lens.lens (\Input' {name} -> name) (\s@Input' {} a -> s {name = a} :: Input)

-- | A collection of key-value pairs.
input_tags :: Lens.Lens' Input (Core.Maybe (Core.HashMap Core.Text Core.Text))
input_tags = Lens.lens (\Input' {tags} -> tags) (\s@Input' {} a -> s {tags = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
input_type :: Lens.Lens' Input (Core.Maybe InputType)
input_type = Lens.lens (\Input' {type'} -> type') (\s@Input' {} a -> s {type' = a} :: Input)

-- | STANDARD - MediaLive expects two sources to be connected to this input.
-- If the channel is also STANDARD, both sources will be ingested. If the
-- channel is SINGLE_PIPELINE, only the first source will be ingested; the
-- second source will always be ignored, even if the first source fails.
-- SINGLE_PIPELINE - You can connect only one source to this input. If the
-- ChannelClass is also SINGLE_PIPELINE, this value is valid. If the
-- ChannelClass is STANDARD, this value is not valid because the channel
-- requires two sources in the input.
input_inputClass :: Lens.Lens' Input (Core.Maybe InputClass)
input_inputClass = Lens.lens (\Input' {inputClass} -> inputClass) (\s@Input' {} a -> s {inputClass = a} :: Input)

-- | Settings for the input devices.
input_inputDevices :: Lens.Lens' Input (Core.Maybe [InputDeviceSettings])
input_inputDevices = Lens.lens (\Input' {inputDevices} -> inputDevices) (\s@Input' {} a -> s {inputDevices = a} :: Input) Core.. Lens.mapping Lens._Coerce

-- | A list of channel IDs that that input is attached to (currently an input
-- can only be attached to one channel).
input_attachedChannels :: Lens.Lens' Input (Core.Maybe [Core.Text])
input_attachedChannels = Lens.lens (\Input' {attachedChannels} -> attachedChannels) (\s@Input' {} a -> s {attachedChannels = a} :: Input) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Input where
  parseJSON =
    Core.withObject
      "Input"
      ( \x ->
          Input'
            Core.<$> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "sources" Core..!= Core.mempty)
            Core.<*> (x Core..:? "inputPartnerIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "inputSourceType")
            Core.<*> (x Core..:? "mediaConnectFlows" Core..!= Core.mempty)
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "securityGroups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "destinations" Core..!= Core.mempty)
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "type")
            Core.<*> (x Core..:? "inputClass")
            Core.<*> (x Core..:? "inputDevices" Core..!= Core.mempty)
            Core.<*> (x Core..:? "attachedChannels" Core..!= Core.mempty)
      )

instance Core.Hashable Input

instance Core.NFData Input
