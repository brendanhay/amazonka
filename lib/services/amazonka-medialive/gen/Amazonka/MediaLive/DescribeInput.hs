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
-- Module      : Amazonka.MediaLive.DescribeInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces details about an input
module Amazonka.MediaLive.DescribeInput
  ( -- * Creating a Request
    DescribeInput (..),
    newDescribeInput,

    -- * Request Lenses
    describeInput_inputId,

    -- * Destructuring the Response
    DescribeInputResponse (..),
    newDescribeInputResponse,

    -- * Response Lenses
    describeInputResponse_tags,
    describeInputResponse_sources,
    describeInputResponse_name,
    describeInputResponse_type,
    describeInputResponse_roleArn,
    describeInputResponse_inputDevices,
    describeInputResponse_mediaConnectFlows,
    describeInputResponse_arn,
    describeInputResponse_state,
    describeInputResponse_id,
    describeInputResponse_securityGroups,
    describeInputResponse_attachedChannels,
    describeInputResponse_inputSourceType,
    describeInputResponse_destinations,
    describeInputResponse_inputPartnerIds,
    describeInputResponse_inputClass,
    describeInputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DescribeInputRequest
--
-- /See:/ 'newDescribeInput' smart constructor.
data DescribeInput = DescribeInput'
  { -- | Unique ID of the input
    inputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeInput
newDescribeInput pInputId_ =
  DescribeInput' {inputId = pInputId_}

-- | Unique ID of the input
describeInput_inputId :: Lens.Lens' DescribeInput Prelude.Text
describeInput_inputId = Lens.lens (\DescribeInput' {inputId} -> inputId) (\s@DescribeInput' {} a -> s {inputId = a} :: DescribeInput)

instance Core.AWSRequest DescribeInput where
  type
    AWSResponse DescribeInput =
      DescribeInputResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInputResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "inputDevices" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "mediaConnectFlows"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "securityGroups" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "attachedChannels"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "inputSourceType")
            Prelude.<*> (x Data..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "inputPartnerIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "inputClass")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInput where
  hashWithSalt _salt DescribeInput' {..} =
    _salt `Prelude.hashWithSalt` inputId

instance Prelude.NFData DescribeInput where
  rnf DescribeInput' {..} = Prelude.rnf inputId

instance Data.ToHeaders DescribeInput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeInput where
  toPath DescribeInput' {..} =
    Prelude.mconcat
      ["/prod/inputs/", Data.toBS inputId]

instance Data.ToQuery DescribeInput where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeInputResponse
--
-- /See:/ 'newDescribeInputResponse' smart constructor.
data DescribeInputResponse = DescribeInputResponse'
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
    inputClass :: Prelude.Maybe InputClass,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeInputResponse_tags' - A collection of key-value pairs.
--
-- 'sources', 'describeInputResponse_sources' - A list of the sources of the input (PULL-type).
--
-- 'name', 'describeInputResponse_name' - The user-assigned name (This is a mutable value).
--
-- 'type'', 'describeInputResponse_type' - Undocumented member.
--
-- 'roleArn', 'describeInputResponse_roleArn' - The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
--
-- 'inputDevices', 'describeInputResponse_inputDevices' - Settings for the input devices.
--
-- 'mediaConnectFlows', 'describeInputResponse_mediaConnectFlows' - A list of MediaConnect Flows for this input.
--
-- 'arn', 'describeInputResponse_arn' - The Unique ARN of the input (generated, immutable).
--
-- 'state', 'describeInputResponse_state' - Undocumented member.
--
-- 'id', 'describeInputResponse_id' - The generated ID of the input (unique for user account, immutable).
--
-- 'securityGroups', 'describeInputResponse_securityGroups' - A list of IDs for all the Input Security Groups attached to the input.
--
-- 'attachedChannels', 'describeInputResponse_attachedChannels' - A list of channel IDs that that input is attached to (currently an input
-- can only be attached to one channel).
--
-- 'inputSourceType', 'describeInputResponse_inputSourceType' - Certain pull input sources can be dynamic, meaning that they can have
-- their URL\'s dynamically changes during input switch actions. Presently,
-- this functionality only works with MP4_FILE and TS_FILE inputs.
--
-- 'destinations', 'describeInputResponse_destinations' - A list of the destinations of the input (PUSH-type).
--
-- 'inputPartnerIds', 'describeInputResponse_inputPartnerIds' - A list of IDs for all Inputs which are partners of this one.
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
-- 'httpStatus', 'describeInputResponse_httpStatus' - The response's http status code.
newDescribeInputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInputResponse
newDescribeInputResponse pHttpStatus_ =
  DescribeInputResponse'
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
      inputClass = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of key-value pairs.
describeInputResponse_tags :: Lens.Lens' DescribeInputResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeInputResponse_tags = Lens.lens (\DescribeInputResponse' {tags} -> tags) (\s@DescribeInputResponse' {} a -> s {tags = a} :: DescribeInputResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the sources of the input (PULL-type).
describeInputResponse_sources :: Lens.Lens' DescribeInputResponse (Prelude.Maybe [InputSource])
describeInputResponse_sources = Lens.lens (\DescribeInputResponse' {sources} -> sources) (\s@DescribeInputResponse' {} a -> s {sources = a} :: DescribeInputResponse) Prelude.. Lens.mapping Lens.coerced

-- | The user-assigned name (This is a mutable value).
describeInputResponse_name :: Lens.Lens' DescribeInputResponse (Prelude.Maybe Prelude.Text)
describeInputResponse_name = Lens.lens (\DescribeInputResponse' {name} -> name) (\s@DescribeInputResponse' {} a -> s {name = a} :: DescribeInputResponse)

-- | Undocumented member.
describeInputResponse_type :: Lens.Lens' DescribeInputResponse (Prelude.Maybe InputType)
describeInputResponse_type = Lens.lens (\DescribeInputResponse' {type'} -> type') (\s@DescribeInputResponse' {} a -> s {type' = a} :: DescribeInputResponse)

-- | The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
describeInputResponse_roleArn :: Lens.Lens' DescribeInputResponse (Prelude.Maybe Prelude.Text)
describeInputResponse_roleArn = Lens.lens (\DescribeInputResponse' {roleArn} -> roleArn) (\s@DescribeInputResponse' {} a -> s {roleArn = a} :: DescribeInputResponse)

-- | Settings for the input devices.
describeInputResponse_inputDevices :: Lens.Lens' DescribeInputResponse (Prelude.Maybe [InputDeviceSettings])
describeInputResponse_inputDevices = Lens.lens (\DescribeInputResponse' {inputDevices} -> inputDevices) (\s@DescribeInputResponse' {} a -> s {inputDevices = a} :: DescribeInputResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of MediaConnect Flows for this input.
describeInputResponse_mediaConnectFlows :: Lens.Lens' DescribeInputResponse (Prelude.Maybe [MediaConnectFlow])
describeInputResponse_mediaConnectFlows = Lens.lens (\DescribeInputResponse' {mediaConnectFlows} -> mediaConnectFlows) (\s@DescribeInputResponse' {} a -> s {mediaConnectFlows = a} :: DescribeInputResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Unique ARN of the input (generated, immutable).
describeInputResponse_arn :: Lens.Lens' DescribeInputResponse (Prelude.Maybe Prelude.Text)
describeInputResponse_arn = Lens.lens (\DescribeInputResponse' {arn} -> arn) (\s@DescribeInputResponse' {} a -> s {arn = a} :: DescribeInputResponse)

-- | Undocumented member.
describeInputResponse_state :: Lens.Lens' DescribeInputResponse (Prelude.Maybe InputState)
describeInputResponse_state = Lens.lens (\DescribeInputResponse' {state} -> state) (\s@DescribeInputResponse' {} a -> s {state = a} :: DescribeInputResponse)

-- | The generated ID of the input (unique for user account, immutable).
describeInputResponse_id :: Lens.Lens' DescribeInputResponse (Prelude.Maybe Prelude.Text)
describeInputResponse_id = Lens.lens (\DescribeInputResponse' {id} -> id) (\s@DescribeInputResponse' {} a -> s {id = a} :: DescribeInputResponse)

-- | A list of IDs for all the Input Security Groups attached to the input.
describeInputResponse_securityGroups :: Lens.Lens' DescribeInputResponse (Prelude.Maybe [Prelude.Text])
describeInputResponse_securityGroups = Lens.lens (\DescribeInputResponse' {securityGroups} -> securityGroups) (\s@DescribeInputResponse' {} a -> s {securityGroups = a} :: DescribeInputResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of channel IDs that that input is attached to (currently an input
-- can only be attached to one channel).
describeInputResponse_attachedChannels :: Lens.Lens' DescribeInputResponse (Prelude.Maybe [Prelude.Text])
describeInputResponse_attachedChannels = Lens.lens (\DescribeInputResponse' {attachedChannels} -> attachedChannels) (\s@DescribeInputResponse' {} a -> s {attachedChannels = a} :: DescribeInputResponse) Prelude.. Lens.mapping Lens.coerced

-- | Certain pull input sources can be dynamic, meaning that they can have
-- their URL\'s dynamically changes during input switch actions. Presently,
-- this functionality only works with MP4_FILE and TS_FILE inputs.
describeInputResponse_inputSourceType :: Lens.Lens' DescribeInputResponse (Prelude.Maybe InputSourceType)
describeInputResponse_inputSourceType = Lens.lens (\DescribeInputResponse' {inputSourceType} -> inputSourceType) (\s@DescribeInputResponse' {} a -> s {inputSourceType = a} :: DescribeInputResponse)

-- | A list of the destinations of the input (PUSH-type).
describeInputResponse_destinations :: Lens.Lens' DescribeInputResponse (Prelude.Maybe [InputDestination])
describeInputResponse_destinations = Lens.lens (\DescribeInputResponse' {destinations} -> destinations) (\s@DescribeInputResponse' {} a -> s {destinations = a} :: DescribeInputResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of IDs for all Inputs which are partners of this one.
describeInputResponse_inputPartnerIds :: Lens.Lens' DescribeInputResponse (Prelude.Maybe [Prelude.Text])
describeInputResponse_inputPartnerIds = Lens.lens (\DescribeInputResponse' {inputPartnerIds} -> inputPartnerIds) (\s@DescribeInputResponse' {} a -> s {inputPartnerIds = a} :: DescribeInputResponse) Prelude.. Lens.mapping Lens.coerced

-- | STANDARD - MediaLive expects two sources to be connected to this input.
-- If the channel is also STANDARD, both sources will be ingested. If the
-- channel is SINGLE_PIPELINE, only the first source will be ingested; the
-- second source will always be ignored, even if the first source fails.
-- SINGLE_PIPELINE - You can connect only one source to this input. If the
-- ChannelClass is also SINGLE_PIPELINE, this value is valid. If the
-- ChannelClass is STANDARD, this value is not valid because the channel
-- requires two sources in the input.
describeInputResponse_inputClass :: Lens.Lens' DescribeInputResponse (Prelude.Maybe InputClass)
describeInputResponse_inputClass = Lens.lens (\DescribeInputResponse' {inputClass} -> inputClass) (\s@DescribeInputResponse' {} a -> s {inputClass = a} :: DescribeInputResponse)

-- | The response's http status code.
describeInputResponse_httpStatus :: Lens.Lens' DescribeInputResponse Prelude.Int
describeInputResponse_httpStatus = Lens.lens (\DescribeInputResponse' {httpStatus} -> httpStatus) (\s@DescribeInputResponse' {} a -> s {httpStatus = a} :: DescribeInputResponse)

instance Prelude.NFData DescribeInputResponse where
  rnf DescribeInputResponse' {..} =
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
      `Prelude.seq` Prelude.rnf httpStatus
