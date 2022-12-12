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
-- Module      : Amazonka.MediaLive.CreateInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an input
module Amazonka.MediaLive.CreateInput
  ( -- * Creating a Request
    CreateInput' (..),
    newCreateInput',

    -- * Request Lenses
    createInput'_destinations,
    createInput'_inputDevices,
    createInput'_inputSecurityGroups,
    createInput'_mediaConnectFlows,
    createInput'_name,
    createInput'_requestId,
    createInput'_roleArn,
    createInput'_sources,
    createInput'_tags,
    createInput'_type,
    createInput'_vpc,

    -- * Destructuring the Response
    CreateInputResponse (..),
    newCreateInputResponse,

    -- * Response Lenses
    createInputResponse_input,
    createInputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The name of the input
--
-- /See:/ 'newCreateInput'' smart constructor.
data CreateInput' = CreateInput''
  { -- | Destination settings for PUSH type inputs.
    destinations :: Prelude.Maybe [InputDestinationRequest],
    -- | Settings for the devices.
    inputDevices :: Prelude.Maybe [InputDeviceSettings],
    -- | A list of security groups referenced by IDs to attach to the input.
    inputSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | A list of the MediaConnect Flows that you want to use in this input. You
    -- can specify as few as one Flow and presently, as many as two. The only
    -- requirement is when you have more than one is that each Flow is in a
    -- separate Availability Zone as this ensures your EML input is redundant
    -- to AZ issues.
    mediaConnectFlows :: Prelude.Maybe [MediaConnectFlowRequest],
    -- | Name of the input.
    name :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier of the request to ensure the request is handled
    -- exactly once in case of retries.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role this input assumes during and
    -- after creation.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The source URLs for a PULL-type input. Every PULL type input needs
    -- exactly two source URLs for redundancy. Only specify sources for PULL
    -- type Inputs. Leave Destinations empty.
    sources :: Prelude.Maybe [InputSourceRequest],
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    type' :: Prelude.Maybe InputType,
    vpc :: Prelude.Maybe InputVpcRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInput'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinations', 'createInput'_destinations' - Destination settings for PUSH type inputs.
--
-- 'inputDevices', 'createInput'_inputDevices' - Settings for the devices.
--
-- 'inputSecurityGroups', 'createInput'_inputSecurityGroups' - A list of security groups referenced by IDs to attach to the input.
--
-- 'mediaConnectFlows', 'createInput'_mediaConnectFlows' - A list of the MediaConnect Flows that you want to use in this input. You
-- can specify as few as one Flow and presently, as many as two. The only
-- requirement is when you have more than one is that each Flow is in a
-- separate Availability Zone as this ensures your EML input is redundant
-- to AZ issues.
--
-- 'name', 'createInput'_name' - Name of the input.
--
-- 'requestId', 'createInput'_requestId' - Unique identifier of the request to ensure the request is handled
-- exactly once in case of retries.
--
-- 'roleArn', 'createInput'_roleArn' - The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
--
-- 'sources', 'createInput'_sources' - The source URLs for a PULL-type input. Every PULL type input needs
-- exactly two source URLs for redundancy. Only specify sources for PULL
-- type Inputs. Leave Destinations empty.
--
-- 'tags', 'createInput'_tags' - A collection of key-value pairs.
--
-- 'type'', 'createInput'_type' - Undocumented member.
--
-- 'vpc', 'createInput'_vpc' - Undocumented member.
newCreateInput' ::
  CreateInput'
newCreateInput' =
  CreateInput''
    { destinations = Prelude.Nothing,
      inputDevices = Prelude.Nothing,
      inputSecurityGroups = Prelude.Nothing,
      mediaConnectFlows = Prelude.Nothing,
      name = Prelude.Nothing,
      requestId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      sources = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | Destination settings for PUSH type inputs.
createInput'_destinations :: Lens.Lens' CreateInput' (Prelude.Maybe [InputDestinationRequest])
createInput'_destinations = Lens.lens (\CreateInput'' {destinations} -> destinations) (\s@CreateInput'' {} a -> s {destinations = a} :: CreateInput') Prelude.. Lens.mapping Lens.coerced

-- | Settings for the devices.
createInput'_inputDevices :: Lens.Lens' CreateInput' (Prelude.Maybe [InputDeviceSettings])
createInput'_inputDevices = Lens.lens (\CreateInput'' {inputDevices} -> inputDevices) (\s@CreateInput'' {} a -> s {inputDevices = a} :: CreateInput') Prelude.. Lens.mapping Lens.coerced

-- | A list of security groups referenced by IDs to attach to the input.
createInput'_inputSecurityGroups :: Lens.Lens' CreateInput' (Prelude.Maybe [Prelude.Text])
createInput'_inputSecurityGroups = Lens.lens (\CreateInput'' {inputSecurityGroups} -> inputSecurityGroups) (\s@CreateInput'' {} a -> s {inputSecurityGroups = a} :: CreateInput') Prelude.. Lens.mapping Lens.coerced

-- | A list of the MediaConnect Flows that you want to use in this input. You
-- can specify as few as one Flow and presently, as many as two. The only
-- requirement is when you have more than one is that each Flow is in a
-- separate Availability Zone as this ensures your EML input is redundant
-- to AZ issues.
createInput'_mediaConnectFlows :: Lens.Lens' CreateInput' (Prelude.Maybe [MediaConnectFlowRequest])
createInput'_mediaConnectFlows = Lens.lens (\CreateInput'' {mediaConnectFlows} -> mediaConnectFlows) (\s@CreateInput'' {} a -> s {mediaConnectFlows = a} :: CreateInput') Prelude.. Lens.mapping Lens.coerced

-- | Name of the input.
createInput'_name :: Lens.Lens' CreateInput' (Prelude.Maybe Prelude.Text)
createInput'_name = Lens.lens (\CreateInput'' {name} -> name) (\s@CreateInput'' {} a -> s {name = a} :: CreateInput')

-- | Unique identifier of the request to ensure the request is handled
-- exactly once in case of retries.
createInput'_requestId :: Lens.Lens' CreateInput' (Prelude.Maybe Prelude.Text)
createInput'_requestId = Lens.lens (\CreateInput'' {requestId} -> requestId) (\s@CreateInput'' {} a -> s {requestId = a} :: CreateInput')

-- | The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
createInput'_roleArn :: Lens.Lens' CreateInput' (Prelude.Maybe Prelude.Text)
createInput'_roleArn = Lens.lens (\CreateInput'' {roleArn} -> roleArn) (\s@CreateInput'' {} a -> s {roleArn = a} :: CreateInput')

-- | The source URLs for a PULL-type input. Every PULL type input needs
-- exactly two source URLs for redundancy. Only specify sources for PULL
-- type Inputs. Leave Destinations empty.
createInput'_sources :: Lens.Lens' CreateInput' (Prelude.Maybe [InputSourceRequest])
createInput'_sources = Lens.lens (\CreateInput'' {sources} -> sources) (\s@CreateInput'' {} a -> s {sources = a} :: CreateInput') Prelude.. Lens.mapping Lens.coerced

-- | A collection of key-value pairs.
createInput'_tags :: Lens.Lens' CreateInput' (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createInput'_tags = Lens.lens (\CreateInput'' {tags} -> tags) (\s@CreateInput'' {} a -> s {tags = a} :: CreateInput') Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createInput'_type :: Lens.Lens' CreateInput' (Prelude.Maybe InputType)
createInput'_type = Lens.lens (\CreateInput'' {type'} -> type') (\s@CreateInput'' {} a -> s {type' = a} :: CreateInput')

-- | Undocumented member.
createInput'_vpc :: Lens.Lens' CreateInput' (Prelude.Maybe InputVpcRequest)
createInput'_vpc = Lens.lens (\CreateInput'' {vpc} -> vpc) (\s@CreateInput'' {} a -> s {vpc = a} :: CreateInput')

instance Core.AWSRequest CreateInput' where
  type AWSResponse CreateInput' = CreateInputResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInputResponse'
            Prelude.<$> (x Data..?> "input")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInput' where
  hashWithSalt _salt CreateInput'' {..} =
    _salt `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` inputDevices
      `Prelude.hashWithSalt` inputSecurityGroups
      `Prelude.hashWithSalt` mediaConnectFlows
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData CreateInput' where
  rnf CreateInput'' {..} =
    Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf inputDevices
      `Prelude.seq` Prelude.rnf inputSecurityGroups
      `Prelude.seq` Prelude.rnf mediaConnectFlows
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf vpc

instance Data.ToHeaders CreateInput' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateInput' where
  toJSON CreateInput'' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destinations" Data..=) Prelude.<$> destinations,
            ("inputDevices" Data..=) Prelude.<$> inputDevices,
            ("inputSecurityGroups" Data..=)
              Prelude.<$> inputSecurityGroups,
            ("mediaConnectFlows" Data..=)
              Prelude.<$> mediaConnectFlows,
            ("name" Data..=) Prelude.<$> name,
            ("requestId" Data..=) Prelude.<$> requestId,
            ("roleArn" Data..=) Prelude.<$> roleArn,
            ("sources" Data..=) Prelude.<$> sources,
            ("tags" Data..=) Prelude.<$> tags,
            ("type" Data..=) Prelude.<$> type',
            ("vpc" Data..=) Prelude.<$> vpc
          ]
      )

instance Data.ToPath CreateInput' where
  toPath = Prelude.const "/prod/inputs"

instance Data.ToQuery CreateInput' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for CreateInputResponse
--
-- /See:/ 'newCreateInputResponse' smart constructor.
data CreateInputResponse = CreateInputResponse'
  { input :: Prelude.Maybe Input,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateInputResponse
newCreateInputResponse pHttpStatus_ =
  CreateInputResponse'
    { input = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createInputResponse_input :: Lens.Lens' CreateInputResponse (Prelude.Maybe Input)
createInputResponse_input = Lens.lens (\CreateInputResponse' {input} -> input) (\s@CreateInputResponse' {} a -> s {input = a} :: CreateInputResponse)

-- | The response's http status code.
createInputResponse_httpStatus :: Lens.Lens' CreateInputResponse Prelude.Int
createInputResponse_httpStatus = Lens.lens (\CreateInputResponse' {httpStatus} -> httpStatus) (\s@CreateInputResponse' {} a -> s {httpStatus = a} :: CreateInputResponse)

instance Prelude.NFData CreateInputResponse where
  rnf CreateInputResponse' {..} =
    Prelude.rnf input
      `Prelude.seq` Prelude.rnf httpStatus
