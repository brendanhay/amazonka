{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces details about an input
module Network.AWS.MediaLive.DescribeInput
  ( -- * Creating a Request
    DescribeInput (..),
    newDescribeInput,

    -- * Request Lenses
    describeInput_inputId,

    -- * Destructuring the Response
    DescribeInputResponse (..),
    newDescribeInputResponse,

    -- * Response Lenses
    describeInputResponse_roleArn,
    describeInputResponse_sources,
    describeInputResponse_inputPartnerIds,
    describeInputResponse_inputSourceType,
    describeInputResponse_mediaConnectFlows,
    describeInputResponse_arn,
    describeInputResponse_id,
    describeInputResponse_securityGroups,
    describeInputResponse_destinations,
    describeInputResponse_state,
    describeInputResponse_name,
    describeInputResponse_tags,
    describeInputResponse_type,
    describeInputResponse_inputClass,
    describeInputResponse_inputDevices,
    describeInputResponse_attachedChannels,
    describeInputResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeInputRequest
--
-- /See:/ 'newDescribeInput' smart constructor.
data DescribeInput = DescribeInput'
  { -- | Unique ID of the input
    inputId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputId', 'describeInput_inputId' - Unique ID of the input
newDescribeInput ::
  -- | 'inputId'
  Core.Text ->
  DescribeInput
newDescribeInput pInputId_ =
  DescribeInput' {inputId = pInputId_}

-- | Unique ID of the input
describeInput_inputId :: Lens.Lens' DescribeInput Core.Text
describeInput_inputId = Lens.lens (\DescribeInput' {inputId} -> inputId) (\s@DescribeInput' {} a -> s {inputId = a} :: DescribeInput)

instance Core.AWSRequest DescribeInput where
  type
    AWSResponse DescribeInput =
      DescribeInputResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInputResponse'
            Core.<$> (x Core..?> "roleArn")
            Core.<*> (x Core..?> "sources" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "inputPartnerIds" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "inputSourceType")
            Core.<*> (x Core..?> "mediaConnectFlows" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "securityGroups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "destinations" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "state")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "type")
            Core.<*> (x Core..?> "inputClass")
            Core.<*> (x Core..?> "inputDevices" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "attachedChannels" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInput

instance Core.NFData DescribeInput

instance Core.ToHeaders DescribeInput where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeInput where
  toPath DescribeInput' {..} =
    Core.mconcat ["/prod/inputs/", Core.toBS inputId]

instance Core.ToQuery DescribeInput where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DescribeInputResponse
--
-- /See:/ 'newDescribeInputResponse' smart constructor.
data DescribeInputResponse = DescribeInputResponse'
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
    attachedChannels :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'describeInputResponse_roleArn' - The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
--
-- 'sources', 'describeInputResponse_sources' - A list of the sources of the input (PULL-type).
--
-- 'inputPartnerIds', 'describeInputResponse_inputPartnerIds' - A list of IDs for all Inputs which are partners of this one.
--
-- 'inputSourceType', 'describeInputResponse_inputSourceType' - Certain pull input sources can be dynamic, meaning that they can have
-- their URL\'s dynamically changes during input switch actions. Presently,
-- this functionality only works with MP4_FILE inputs.
--
-- 'mediaConnectFlows', 'describeInputResponse_mediaConnectFlows' - A list of MediaConnect Flows for this input.
--
-- 'arn', 'describeInputResponse_arn' - The Unique ARN of the input (generated, immutable).
--
-- 'id', 'describeInputResponse_id' - The generated ID of the input (unique for user account, immutable).
--
-- 'securityGroups', 'describeInputResponse_securityGroups' - A list of IDs for all the Input Security Groups attached to the input.
--
-- 'destinations', 'describeInputResponse_destinations' - A list of the destinations of the input (PUSH-type).
--
-- 'state', 'describeInputResponse_state' - Undocumented member.
--
-- 'name', 'describeInputResponse_name' - The user-assigned name (This is a mutable value).
--
-- 'tags', 'describeInputResponse_tags' - A collection of key-value pairs.
--
-- 'type'', 'describeInputResponse_type' - Undocumented member.
--
-- 'inputClass', 'describeInputResponse_inputClass' - STANDARD - MediaLive expects two sources to be connected to this input.
-- If the channel is also STANDARD, both sources will be ingested. If the
-- channel is SINGLE_PIPELINE, only the first source will be ingested; the
-- second source will always be ignored, even if the first source fails.
-- SINGLE_PIPELINE - You can connect only one source to this input. If the
-- ChannelClass is also SINGLE_PIPELINE, this value is valid. If the
-- ChannelClass is STANDARD, this value is not valid because the channel
-- requires two sources in the input.
--
-- 'inputDevices', 'describeInputResponse_inputDevices' - Settings for the input devices.
--
-- 'attachedChannels', 'describeInputResponse_attachedChannels' - A list of channel IDs that that input is attached to (currently an input
-- can only be attached to one channel).
--
-- 'httpStatus', 'describeInputResponse_httpStatus' - The response's http status code.
newDescribeInputResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInputResponse
newDescribeInputResponse pHttpStatus_ =
  DescribeInputResponse'
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
      attachedChannels = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
describeInputResponse_roleArn :: Lens.Lens' DescribeInputResponse (Core.Maybe Core.Text)
describeInputResponse_roleArn = Lens.lens (\DescribeInputResponse' {roleArn} -> roleArn) (\s@DescribeInputResponse' {} a -> s {roleArn = a} :: DescribeInputResponse)

-- | A list of the sources of the input (PULL-type).
describeInputResponse_sources :: Lens.Lens' DescribeInputResponse (Core.Maybe [InputSource])
describeInputResponse_sources = Lens.lens (\DescribeInputResponse' {sources} -> sources) (\s@DescribeInputResponse' {} a -> s {sources = a} :: DescribeInputResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of IDs for all Inputs which are partners of this one.
describeInputResponse_inputPartnerIds :: Lens.Lens' DescribeInputResponse (Core.Maybe [Core.Text])
describeInputResponse_inputPartnerIds = Lens.lens (\DescribeInputResponse' {inputPartnerIds} -> inputPartnerIds) (\s@DescribeInputResponse' {} a -> s {inputPartnerIds = a} :: DescribeInputResponse) Core.. Lens.mapping Lens._Coerce

-- | Certain pull input sources can be dynamic, meaning that they can have
-- their URL\'s dynamically changes during input switch actions. Presently,
-- this functionality only works with MP4_FILE inputs.
describeInputResponse_inputSourceType :: Lens.Lens' DescribeInputResponse (Core.Maybe InputSourceType)
describeInputResponse_inputSourceType = Lens.lens (\DescribeInputResponse' {inputSourceType} -> inputSourceType) (\s@DescribeInputResponse' {} a -> s {inputSourceType = a} :: DescribeInputResponse)

-- | A list of MediaConnect Flows for this input.
describeInputResponse_mediaConnectFlows :: Lens.Lens' DescribeInputResponse (Core.Maybe [MediaConnectFlow])
describeInputResponse_mediaConnectFlows = Lens.lens (\DescribeInputResponse' {mediaConnectFlows} -> mediaConnectFlows) (\s@DescribeInputResponse' {} a -> s {mediaConnectFlows = a} :: DescribeInputResponse) Core.. Lens.mapping Lens._Coerce

-- | The Unique ARN of the input (generated, immutable).
describeInputResponse_arn :: Lens.Lens' DescribeInputResponse (Core.Maybe Core.Text)
describeInputResponse_arn = Lens.lens (\DescribeInputResponse' {arn} -> arn) (\s@DescribeInputResponse' {} a -> s {arn = a} :: DescribeInputResponse)

-- | The generated ID of the input (unique for user account, immutable).
describeInputResponse_id :: Lens.Lens' DescribeInputResponse (Core.Maybe Core.Text)
describeInputResponse_id = Lens.lens (\DescribeInputResponse' {id} -> id) (\s@DescribeInputResponse' {} a -> s {id = a} :: DescribeInputResponse)

-- | A list of IDs for all the Input Security Groups attached to the input.
describeInputResponse_securityGroups :: Lens.Lens' DescribeInputResponse (Core.Maybe [Core.Text])
describeInputResponse_securityGroups = Lens.lens (\DescribeInputResponse' {securityGroups} -> securityGroups) (\s@DescribeInputResponse' {} a -> s {securityGroups = a} :: DescribeInputResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of the destinations of the input (PUSH-type).
describeInputResponse_destinations :: Lens.Lens' DescribeInputResponse (Core.Maybe [InputDestination])
describeInputResponse_destinations = Lens.lens (\DescribeInputResponse' {destinations} -> destinations) (\s@DescribeInputResponse' {} a -> s {destinations = a} :: DescribeInputResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
describeInputResponse_state :: Lens.Lens' DescribeInputResponse (Core.Maybe InputState)
describeInputResponse_state = Lens.lens (\DescribeInputResponse' {state} -> state) (\s@DescribeInputResponse' {} a -> s {state = a} :: DescribeInputResponse)

-- | The user-assigned name (This is a mutable value).
describeInputResponse_name :: Lens.Lens' DescribeInputResponse (Core.Maybe Core.Text)
describeInputResponse_name = Lens.lens (\DescribeInputResponse' {name} -> name) (\s@DescribeInputResponse' {} a -> s {name = a} :: DescribeInputResponse)

-- | A collection of key-value pairs.
describeInputResponse_tags :: Lens.Lens' DescribeInputResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeInputResponse_tags = Lens.lens (\DescribeInputResponse' {tags} -> tags) (\s@DescribeInputResponse' {} a -> s {tags = a} :: DescribeInputResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
describeInputResponse_type :: Lens.Lens' DescribeInputResponse (Core.Maybe InputType)
describeInputResponse_type = Lens.lens (\DescribeInputResponse' {type'} -> type') (\s@DescribeInputResponse' {} a -> s {type' = a} :: DescribeInputResponse)

-- | STANDARD - MediaLive expects two sources to be connected to this input.
-- If the channel is also STANDARD, both sources will be ingested. If the
-- channel is SINGLE_PIPELINE, only the first source will be ingested; the
-- second source will always be ignored, even if the first source fails.
-- SINGLE_PIPELINE - You can connect only one source to this input. If the
-- ChannelClass is also SINGLE_PIPELINE, this value is valid. If the
-- ChannelClass is STANDARD, this value is not valid because the channel
-- requires two sources in the input.
describeInputResponse_inputClass :: Lens.Lens' DescribeInputResponse (Core.Maybe InputClass)
describeInputResponse_inputClass = Lens.lens (\DescribeInputResponse' {inputClass} -> inputClass) (\s@DescribeInputResponse' {} a -> s {inputClass = a} :: DescribeInputResponse)

-- | Settings for the input devices.
describeInputResponse_inputDevices :: Lens.Lens' DescribeInputResponse (Core.Maybe [InputDeviceSettings])
describeInputResponse_inputDevices = Lens.lens (\DescribeInputResponse' {inputDevices} -> inputDevices) (\s@DescribeInputResponse' {} a -> s {inputDevices = a} :: DescribeInputResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of channel IDs that that input is attached to (currently an input
-- can only be attached to one channel).
describeInputResponse_attachedChannels :: Lens.Lens' DescribeInputResponse (Core.Maybe [Core.Text])
describeInputResponse_attachedChannels = Lens.lens (\DescribeInputResponse' {attachedChannels} -> attachedChannels) (\s@DescribeInputResponse' {} a -> s {attachedChannels = a} :: DescribeInputResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInputResponse_httpStatus :: Lens.Lens' DescribeInputResponse Core.Int
describeInputResponse_httpStatus = Lens.lens (\DescribeInputResponse' {httpStatus} -> httpStatus) (\s@DescribeInputResponse' {} a -> s {httpStatus = a} :: DescribeInputResponse)

instance Core.NFData DescribeInputResponse
