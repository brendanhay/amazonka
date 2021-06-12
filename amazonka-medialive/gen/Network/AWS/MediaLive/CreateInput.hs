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
-- Module      : Network.AWS.MediaLive.CreateInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an input
module Network.AWS.MediaLive.CreateInput
  ( -- * Creating a Request
    CreateInput' (..),
    newCreateInput',

    -- * Request Lenses
    createInput'_inputSecurityGroups,
    createInput'_roleArn,
    createInput'_sources,
    createInput'_mediaConnectFlows,
    createInput'_destinations,
    createInput'_name,
    createInput'_requestId,
    createInput'_tags,
    createInput'_type,
    createInput'_vpc,
    createInput'_inputDevices,

    -- * Destructuring the Response
    CreateInputResponse (..),
    newCreateInputResponse,

    -- * Response Lenses
    createInputResponse_input,
    createInputResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The name of the input
--
-- /See:/ 'newCreateInput'' smart constructor.
data CreateInput' = CreateInput''
  { -- | A list of security groups referenced by IDs to attach to the input.
    inputSecurityGroups :: Core.Maybe [Core.Text],
    -- | The Amazon Resource Name (ARN) of the role this input assumes during and
    -- after creation.
    roleArn :: Core.Maybe Core.Text,
    -- | The source URLs for a PULL-type input. Every PULL type input needs
    -- exactly two source URLs for redundancy. Only specify sources for PULL
    -- type Inputs. Leave Destinations empty.
    sources :: Core.Maybe [InputSourceRequest],
    -- | A list of the MediaConnect Flows that you want to use in this input. You
    -- can specify as few as one Flow and presently, as many as two. The only
    -- requirement is when you have more than one is that each Flow is in a
    -- separate Availability Zone as this ensures your EML input is redundant
    -- to AZ issues.
    mediaConnectFlows :: Core.Maybe [MediaConnectFlowRequest],
    -- | Destination settings for PUSH type inputs.
    destinations :: Core.Maybe [InputDestinationRequest],
    -- | Name of the input.
    name :: Core.Maybe Core.Text,
    -- | Unique identifier of the request to ensure the request is handled
    -- exactly once in case of retries.
    requestId :: Core.Maybe Core.Text,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    type' :: Core.Maybe InputType,
    vpc :: Core.Maybe InputVpcRequest,
    -- | Settings for the devices.
    inputDevices :: Core.Maybe [InputDeviceSettings]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInput'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSecurityGroups', 'createInput'_inputSecurityGroups' - A list of security groups referenced by IDs to attach to the input.
--
-- 'roleArn', 'createInput'_roleArn' - The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
--
-- 'sources', 'createInput'_sources' - The source URLs for a PULL-type input. Every PULL type input needs
-- exactly two source URLs for redundancy. Only specify sources for PULL
-- type Inputs. Leave Destinations empty.
--
-- 'mediaConnectFlows', 'createInput'_mediaConnectFlows' - A list of the MediaConnect Flows that you want to use in this input. You
-- can specify as few as one Flow and presently, as many as two. The only
-- requirement is when you have more than one is that each Flow is in a
-- separate Availability Zone as this ensures your EML input is redundant
-- to AZ issues.
--
-- 'destinations', 'createInput'_destinations' - Destination settings for PUSH type inputs.
--
-- 'name', 'createInput'_name' - Name of the input.
--
-- 'requestId', 'createInput'_requestId' - Unique identifier of the request to ensure the request is handled
-- exactly once in case of retries.
--
-- 'tags', 'createInput'_tags' - A collection of key-value pairs.
--
-- 'type'', 'createInput'_type' - Undocumented member.
--
-- 'vpc', 'createInput'_vpc' - Undocumented member.
--
-- 'inputDevices', 'createInput'_inputDevices' - Settings for the devices.
newCreateInput' ::
  CreateInput'
newCreateInput' =
  CreateInput''
    { inputSecurityGroups = Core.Nothing,
      roleArn = Core.Nothing,
      sources = Core.Nothing,
      mediaConnectFlows = Core.Nothing,
      destinations = Core.Nothing,
      name = Core.Nothing,
      requestId = Core.Nothing,
      tags = Core.Nothing,
      type' = Core.Nothing,
      vpc = Core.Nothing,
      inputDevices = Core.Nothing
    }

-- | A list of security groups referenced by IDs to attach to the input.
createInput'_inputSecurityGroups :: Lens.Lens' CreateInput' (Core.Maybe [Core.Text])
createInput'_inputSecurityGroups = Lens.lens (\CreateInput'' {inputSecurityGroups} -> inputSecurityGroups) (\s@CreateInput'' {} a -> s {inputSecurityGroups = a} :: CreateInput') Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
createInput'_roleArn :: Lens.Lens' CreateInput' (Core.Maybe Core.Text)
createInput'_roleArn = Lens.lens (\CreateInput'' {roleArn} -> roleArn) (\s@CreateInput'' {} a -> s {roleArn = a} :: CreateInput')

-- | The source URLs for a PULL-type input. Every PULL type input needs
-- exactly two source URLs for redundancy. Only specify sources for PULL
-- type Inputs. Leave Destinations empty.
createInput'_sources :: Lens.Lens' CreateInput' (Core.Maybe [InputSourceRequest])
createInput'_sources = Lens.lens (\CreateInput'' {sources} -> sources) (\s@CreateInput'' {} a -> s {sources = a} :: CreateInput') Core.. Lens.mapping Lens._Coerce

-- | A list of the MediaConnect Flows that you want to use in this input. You
-- can specify as few as one Flow and presently, as many as two. The only
-- requirement is when you have more than one is that each Flow is in a
-- separate Availability Zone as this ensures your EML input is redundant
-- to AZ issues.
createInput'_mediaConnectFlows :: Lens.Lens' CreateInput' (Core.Maybe [MediaConnectFlowRequest])
createInput'_mediaConnectFlows = Lens.lens (\CreateInput'' {mediaConnectFlows} -> mediaConnectFlows) (\s@CreateInput'' {} a -> s {mediaConnectFlows = a} :: CreateInput') Core.. Lens.mapping Lens._Coerce

-- | Destination settings for PUSH type inputs.
createInput'_destinations :: Lens.Lens' CreateInput' (Core.Maybe [InputDestinationRequest])
createInput'_destinations = Lens.lens (\CreateInput'' {destinations} -> destinations) (\s@CreateInput'' {} a -> s {destinations = a} :: CreateInput') Core.. Lens.mapping Lens._Coerce

-- | Name of the input.
createInput'_name :: Lens.Lens' CreateInput' (Core.Maybe Core.Text)
createInput'_name = Lens.lens (\CreateInput'' {name} -> name) (\s@CreateInput'' {} a -> s {name = a} :: CreateInput')

-- | Unique identifier of the request to ensure the request is handled
-- exactly once in case of retries.
createInput'_requestId :: Lens.Lens' CreateInput' (Core.Maybe Core.Text)
createInput'_requestId = Lens.lens (\CreateInput'' {requestId} -> requestId) (\s@CreateInput'' {} a -> s {requestId = a} :: CreateInput')

-- | A collection of key-value pairs.
createInput'_tags :: Lens.Lens' CreateInput' (Core.Maybe (Core.HashMap Core.Text Core.Text))
createInput'_tags = Lens.lens (\CreateInput'' {tags} -> tags) (\s@CreateInput'' {} a -> s {tags = a} :: CreateInput') Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
createInput'_type :: Lens.Lens' CreateInput' (Core.Maybe InputType)
createInput'_type = Lens.lens (\CreateInput'' {type'} -> type') (\s@CreateInput'' {} a -> s {type' = a} :: CreateInput')

-- | Undocumented member.
createInput'_vpc :: Lens.Lens' CreateInput' (Core.Maybe InputVpcRequest)
createInput'_vpc = Lens.lens (\CreateInput'' {vpc} -> vpc) (\s@CreateInput'' {} a -> s {vpc = a} :: CreateInput')

-- | Settings for the devices.
createInput'_inputDevices :: Lens.Lens' CreateInput' (Core.Maybe [InputDeviceSettings])
createInput'_inputDevices = Lens.lens (\CreateInput'' {inputDevices} -> inputDevices) (\s@CreateInput'' {} a -> s {inputDevices = a} :: CreateInput') Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest CreateInput' where
  type AWSResponse CreateInput' = CreateInputResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInputResponse'
            Core.<$> (x Core..?> "input")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateInput'

instance Core.NFData CreateInput'

instance Core.ToHeaders CreateInput' where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateInput' where
  toJSON CreateInput'' {..} =
    Core.object
      ( Core.catMaybes
          [ ("inputSecurityGroups" Core..=)
              Core.<$> inputSecurityGroups,
            ("roleArn" Core..=) Core.<$> roleArn,
            ("sources" Core..=) Core.<$> sources,
            ("mediaConnectFlows" Core..=)
              Core.<$> mediaConnectFlows,
            ("destinations" Core..=) Core.<$> destinations,
            ("name" Core..=) Core.<$> name,
            ("requestId" Core..=) Core.<$> requestId,
            ("tags" Core..=) Core.<$> tags,
            ("type" Core..=) Core.<$> type',
            ("vpc" Core..=) Core.<$> vpc,
            ("inputDevices" Core..=) Core.<$> inputDevices
          ]
      )

instance Core.ToPath CreateInput' where
  toPath = Core.const "/prod/inputs"

instance Core.ToQuery CreateInput' where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for CreateInputResponse
--
-- /See:/ 'newCreateInputResponse' smart constructor.
data CreateInputResponse = CreateInputResponse'
  { input :: Core.Maybe Input,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'createInputResponse_input' - Undocumented member.
--
-- 'httpStatus', 'createInputResponse_httpStatus' - The response's http status code.
newCreateInputResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateInputResponse
newCreateInputResponse pHttpStatus_ =
  CreateInputResponse'
    { input = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createInputResponse_input :: Lens.Lens' CreateInputResponse (Core.Maybe Input)
createInputResponse_input = Lens.lens (\CreateInputResponse' {input} -> input) (\s@CreateInputResponse' {} a -> s {input = a} :: CreateInputResponse)

-- | The response's http status code.
createInputResponse_httpStatus :: Lens.Lens' CreateInputResponse Core.Int
createInputResponse_httpStatus = Lens.lens (\CreateInputResponse' {httpStatus} -> httpStatus) (\s@CreateInputResponse' {} a -> s {httpStatus = a} :: CreateInputResponse)

instance Core.NFData CreateInputResponse
