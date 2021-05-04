{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.UpdateInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an input.
module Network.AWS.MediaLive.UpdateInput
  ( -- * Creating a Request
    UpdateInput' (..),
    newUpdateInput',

    -- * Request Lenses
    updateInput'_inputSecurityGroups,
    updateInput'_roleArn,
    updateInput'_sources,
    updateInput'_mediaConnectFlows,
    updateInput'_destinations,
    updateInput'_name,
    updateInput'_inputDevices,
    updateInput'_inputId,

    -- * Destructuring the Response
    UpdateInputResponse (..),
    newUpdateInputResponse,

    -- * Response Lenses
    updateInputResponse_input,
    updateInputResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update an input.
--
-- /See:/ 'newUpdateInput'' smart constructor.
data UpdateInput' = UpdateInput''
  { -- | A list of security groups referenced by IDs to attach to the input.
    inputSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the role this input assumes during and
    -- after creation.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The source URLs for a PULL-type input. Every PULL type input needs
    -- exactly two source URLs for redundancy. Only specify sources for PULL
    -- type Inputs. Leave Destinations empty.
    sources :: Prelude.Maybe [InputSourceRequest],
    -- | A list of the MediaConnect Flow ARNs that you want to use as the source
    -- of the input. You can specify as few as one Flow and presently, as many
    -- as two. The only requirement is when you have more than one is that each
    -- Flow is in a separate Availability Zone as this ensures your EML input
    -- is redundant to AZ issues.
    mediaConnectFlows :: Prelude.Maybe [MediaConnectFlowRequest],
    -- | Destination settings for PUSH type inputs.
    destinations :: Prelude.Maybe [InputDestinationRequest],
    -- | Name of the input.
    name :: Prelude.Maybe Prelude.Text,
    -- | Settings for the devices.
    inputDevices :: Prelude.Maybe [InputDeviceRequest],
    -- | Unique ID of the input.
    inputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateInput'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSecurityGroups', 'updateInput'_inputSecurityGroups' - A list of security groups referenced by IDs to attach to the input.
--
-- 'roleArn', 'updateInput'_roleArn' - The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
--
-- 'sources', 'updateInput'_sources' - The source URLs for a PULL-type input. Every PULL type input needs
-- exactly two source URLs for redundancy. Only specify sources for PULL
-- type Inputs. Leave Destinations empty.
--
-- 'mediaConnectFlows', 'updateInput'_mediaConnectFlows' - A list of the MediaConnect Flow ARNs that you want to use as the source
-- of the input. You can specify as few as one Flow and presently, as many
-- as two. The only requirement is when you have more than one is that each
-- Flow is in a separate Availability Zone as this ensures your EML input
-- is redundant to AZ issues.
--
-- 'destinations', 'updateInput'_destinations' - Destination settings for PUSH type inputs.
--
-- 'name', 'updateInput'_name' - Name of the input.
--
-- 'inputDevices', 'updateInput'_inputDevices' - Settings for the devices.
--
-- 'inputId', 'updateInput'_inputId' - Unique ID of the input.
newUpdateInput' ::
  -- | 'inputId'
  Prelude.Text ->
  UpdateInput'
newUpdateInput' pInputId_ =
  UpdateInput''
    { inputSecurityGroups =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      sources = Prelude.Nothing,
      mediaConnectFlows = Prelude.Nothing,
      destinations = Prelude.Nothing,
      name = Prelude.Nothing,
      inputDevices = Prelude.Nothing,
      inputId = pInputId_
    }

-- | A list of security groups referenced by IDs to attach to the input.
updateInput'_inputSecurityGroups :: Lens.Lens' UpdateInput' (Prelude.Maybe [Prelude.Text])
updateInput'_inputSecurityGroups = Lens.lens (\UpdateInput'' {inputSecurityGroups} -> inputSecurityGroups) (\s@UpdateInput'' {} a -> s {inputSecurityGroups = a} :: UpdateInput') Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
updateInput'_roleArn :: Lens.Lens' UpdateInput' (Prelude.Maybe Prelude.Text)
updateInput'_roleArn = Lens.lens (\UpdateInput'' {roleArn} -> roleArn) (\s@UpdateInput'' {} a -> s {roleArn = a} :: UpdateInput')

-- | The source URLs for a PULL-type input. Every PULL type input needs
-- exactly two source URLs for redundancy. Only specify sources for PULL
-- type Inputs. Leave Destinations empty.
updateInput'_sources :: Lens.Lens' UpdateInput' (Prelude.Maybe [InputSourceRequest])
updateInput'_sources = Lens.lens (\UpdateInput'' {sources} -> sources) (\s@UpdateInput'' {} a -> s {sources = a} :: UpdateInput') Prelude.. Lens.mapping Prelude._Coerce

-- | A list of the MediaConnect Flow ARNs that you want to use as the source
-- of the input. You can specify as few as one Flow and presently, as many
-- as two. The only requirement is when you have more than one is that each
-- Flow is in a separate Availability Zone as this ensures your EML input
-- is redundant to AZ issues.
updateInput'_mediaConnectFlows :: Lens.Lens' UpdateInput' (Prelude.Maybe [MediaConnectFlowRequest])
updateInput'_mediaConnectFlows = Lens.lens (\UpdateInput'' {mediaConnectFlows} -> mediaConnectFlows) (\s@UpdateInput'' {} a -> s {mediaConnectFlows = a} :: UpdateInput') Prelude.. Lens.mapping Prelude._Coerce

-- | Destination settings for PUSH type inputs.
updateInput'_destinations :: Lens.Lens' UpdateInput' (Prelude.Maybe [InputDestinationRequest])
updateInput'_destinations = Lens.lens (\UpdateInput'' {destinations} -> destinations) (\s@UpdateInput'' {} a -> s {destinations = a} :: UpdateInput') Prelude.. Lens.mapping Prelude._Coerce

-- | Name of the input.
updateInput'_name :: Lens.Lens' UpdateInput' (Prelude.Maybe Prelude.Text)
updateInput'_name = Lens.lens (\UpdateInput'' {name} -> name) (\s@UpdateInput'' {} a -> s {name = a} :: UpdateInput')

-- | Settings for the devices.
updateInput'_inputDevices :: Lens.Lens' UpdateInput' (Prelude.Maybe [InputDeviceRequest])
updateInput'_inputDevices = Lens.lens (\UpdateInput'' {inputDevices} -> inputDevices) (\s@UpdateInput'' {} a -> s {inputDevices = a} :: UpdateInput') Prelude.. Lens.mapping Prelude._Coerce

-- | Unique ID of the input.
updateInput'_inputId :: Lens.Lens' UpdateInput' Prelude.Text
updateInput'_inputId = Lens.lens (\UpdateInput'' {inputId} -> inputId) (\s@UpdateInput'' {} a -> s {inputId = a} :: UpdateInput')

instance Prelude.AWSRequest UpdateInput' where
  type Rs UpdateInput' = UpdateInputResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInputResponse'
            Prelude.<$> (x Prelude..?> "input")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateInput'

instance Prelude.NFData UpdateInput'

instance Prelude.ToHeaders UpdateInput' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateInput' where
  toJSON UpdateInput'' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("inputSecurityGroups" Prelude..=)
              Prelude.<$> inputSecurityGroups,
            ("roleArn" Prelude..=) Prelude.<$> roleArn,
            ("sources" Prelude..=) Prelude.<$> sources,
            ("mediaConnectFlows" Prelude..=)
              Prelude.<$> mediaConnectFlows,
            ("destinations" Prelude..=) Prelude.<$> destinations,
            ("name" Prelude..=) Prelude.<$> name,
            ("inputDevices" Prelude..=)
              Prelude.<$> inputDevices
          ]
      )

instance Prelude.ToPath UpdateInput' where
  toPath UpdateInput'' {..} =
    Prelude.mconcat
      ["/prod/inputs/", Prelude.toBS inputId]

instance Prelude.ToQuery UpdateInput' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for UpdateInputResponse
--
-- /See:/ 'newUpdateInputResponse' smart constructor.
data UpdateInputResponse = UpdateInputResponse'
  { input :: Prelude.Maybe Input,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateInputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'updateInputResponse_input' - Undocumented member.
--
-- 'httpStatus', 'updateInputResponse_httpStatus' - The response's http status code.
newUpdateInputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateInputResponse
newUpdateInputResponse pHttpStatus_ =
  UpdateInputResponse'
    { input = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateInputResponse_input :: Lens.Lens' UpdateInputResponse (Prelude.Maybe Input)
updateInputResponse_input = Lens.lens (\UpdateInputResponse' {input} -> input) (\s@UpdateInputResponse' {} a -> s {input = a} :: UpdateInputResponse)

-- | The response's http status code.
updateInputResponse_httpStatus :: Lens.Lens' UpdateInputResponse Prelude.Int
updateInputResponse_httpStatus = Lens.lens (\UpdateInputResponse' {httpStatus} -> httpStatus) (\s@UpdateInputResponse' {} a -> s {httpStatus = a} :: UpdateInputResponse)

instance Prelude.NFData UpdateInputResponse
