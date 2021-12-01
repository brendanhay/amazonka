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
-- Module      : Amazonka.MediaLive.UpdateInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an input.
module Amazonka.MediaLive.UpdateInput
  ( -- * Creating a Request
    UpdateInput' (..),
    newUpdateInput',

    -- * Request Lenses
    updateInput'_inputDevices,
    updateInput'_sources,
    updateInput'_inputSecurityGroups,
    updateInput'_destinations,
    updateInput'_name,
    updateInput'_mediaConnectFlows,
    updateInput'_roleArn,
    updateInput'_inputId,

    -- * Destructuring the Response
    UpdateInputResponse (..),
    newUpdateInputResponse,

    -- * Response Lenses
    updateInputResponse_input,
    updateInputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to update an input.
--
-- /See:/ 'newUpdateInput'' smart constructor.
data UpdateInput' = UpdateInput''
  { -- | Settings for the devices.
    inputDevices :: Prelude.Maybe [InputDeviceRequest],
    -- | The source URLs for a PULL-type input. Every PULL type input needs
    -- exactly two source URLs for redundancy. Only specify sources for PULL
    -- type Inputs. Leave Destinations empty.
    sources :: Prelude.Maybe [InputSourceRequest],
    -- | A list of security groups referenced by IDs to attach to the input.
    inputSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Destination settings for PUSH type inputs.
    destinations :: Prelude.Maybe [InputDestinationRequest],
    -- | Name of the input.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of the MediaConnect Flow ARNs that you want to use as the source
    -- of the input. You can specify as few as one Flow and presently, as many
    -- as two. The only requirement is when you have more than one is that each
    -- Flow is in a separate Availability Zone as this ensures your EML input
    -- is redundant to AZ issues.
    mediaConnectFlows :: Prelude.Maybe [MediaConnectFlowRequest],
    -- | The Amazon Resource Name (ARN) of the role this input assumes during and
    -- after creation.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Unique ID of the input.
    inputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInput'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDevices', 'updateInput'_inputDevices' - Settings for the devices.
--
-- 'sources', 'updateInput'_sources' - The source URLs for a PULL-type input. Every PULL type input needs
-- exactly two source URLs for redundancy. Only specify sources for PULL
-- type Inputs. Leave Destinations empty.
--
-- 'inputSecurityGroups', 'updateInput'_inputSecurityGroups' - A list of security groups referenced by IDs to attach to the input.
--
-- 'destinations', 'updateInput'_destinations' - Destination settings for PUSH type inputs.
--
-- 'name', 'updateInput'_name' - Name of the input.
--
-- 'mediaConnectFlows', 'updateInput'_mediaConnectFlows' - A list of the MediaConnect Flow ARNs that you want to use as the source
-- of the input. You can specify as few as one Flow and presently, as many
-- as two. The only requirement is when you have more than one is that each
-- Flow is in a separate Availability Zone as this ensures your EML input
-- is redundant to AZ issues.
--
-- 'roleArn', 'updateInput'_roleArn' - The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
--
-- 'inputId', 'updateInput'_inputId' - Unique ID of the input.
newUpdateInput' ::
  -- | 'inputId'
  Prelude.Text ->
  UpdateInput'
newUpdateInput' pInputId_ =
  UpdateInput''
    { inputDevices = Prelude.Nothing,
      sources = Prelude.Nothing,
      inputSecurityGroups = Prelude.Nothing,
      destinations = Prelude.Nothing,
      name = Prelude.Nothing,
      mediaConnectFlows = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      inputId = pInputId_
    }

-- | Settings for the devices.
updateInput'_inputDevices :: Lens.Lens' UpdateInput' (Prelude.Maybe [InputDeviceRequest])
updateInput'_inputDevices = Lens.lens (\UpdateInput'' {inputDevices} -> inputDevices) (\s@UpdateInput'' {} a -> s {inputDevices = a} :: UpdateInput') Prelude.. Lens.mapping Lens.coerced

-- | The source URLs for a PULL-type input. Every PULL type input needs
-- exactly two source URLs for redundancy. Only specify sources for PULL
-- type Inputs. Leave Destinations empty.
updateInput'_sources :: Lens.Lens' UpdateInput' (Prelude.Maybe [InputSourceRequest])
updateInput'_sources = Lens.lens (\UpdateInput'' {sources} -> sources) (\s@UpdateInput'' {} a -> s {sources = a} :: UpdateInput') Prelude.. Lens.mapping Lens.coerced

-- | A list of security groups referenced by IDs to attach to the input.
updateInput'_inputSecurityGroups :: Lens.Lens' UpdateInput' (Prelude.Maybe [Prelude.Text])
updateInput'_inputSecurityGroups = Lens.lens (\UpdateInput'' {inputSecurityGroups} -> inputSecurityGroups) (\s@UpdateInput'' {} a -> s {inputSecurityGroups = a} :: UpdateInput') Prelude.. Lens.mapping Lens.coerced

-- | Destination settings for PUSH type inputs.
updateInput'_destinations :: Lens.Lens' UpdateInput' (Prelude.Maybe [InputDestinationRequest])
updateInput'_destinations = Lens.lens (\UpdateInput'' {destinations} -> destinations) (\s@UpdateInput'' {} a -> s {destinations = a} :: UpdateInput') Prelude.. Lens.mapping Lens.coerced

-- | Name of the input.
updateInput'_name :: Lens.Lens' UpdateInput' (Prelude.Maybe Prelude.Text)
updateInput'_name = Lens.lens (\UpdateInput'' {name} -> name) (\s@UpdateInput'' {} a -> s {name = a} :: UpdateInput')

-- | A list of the MediaConnect Flow ARNs that you want to use as the source
-- of the input. You can specify as few as one Flow and presently, as many
-- as two. The only requirement is when you have more than one is that each
-- Flow is in a separate Availability Zone as this ensures your EML input
-- is redundant to AZ issues.
updateInput'_mediaConnectFlows :: Lens.Lens' UpdateInput' (Prelude.Maybe [MediaConnectFlowRequest])
updateInput'_mediaConnectFlows = Lens.lens (\UpdateInput'' {mediaConnectFlows} -> mediaConnectFlows) (\s@UpdateInput'' {} a -> s {mediaConnectFlows = a} :: UpdateInput') Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the role this input assumes during and
-- after creation.
updateInput'_roleArn :: Lens.Lens' UpdateInput' (Prelude.Maybe Prelude.Text)
updateInput'_roleArn = Lens.lens (\UpdateInput'' {roleArn} -> roleArn) (\s@UpdateInput'' {} a -> s {roleArn = a} :: UpdateInput')

-- | Unique ID of the input.
updateInput'_inputId :: Lens.Lens' UpdateInput' Prelude.Text
updateInput'_inputId = Lens.lens (\UpdateInput'' {inputId} -> inputId) (\s@UpdateInput'' {} a -> s {inputId = a} :: UpdateInput')

instance Core.AWSRequest UpdateInput' where
  type AWSResponse UpdateInput' = UpdateInputResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInputResponse'
            Prelude.<$> (x Core..?> "input")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateInput' where
  hashWithSalt salt' UpdateInput'' {..} =
    salt' `Prelude.hashWithSalt` inputId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` mediaConnectFlows
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` inputSecurityGroups
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` inputDevices

instance Prelude.NFData UpdateInput' where
  rnf UpdateInput'' {..} =
    Prelude.rnf inputDevices
      `Prelude.seq` Prelude.rnf inputId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf mediaConnectFlows
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf inputSecurityGroups
      `Prelude.seq` Prelude.rnf sources

instance Core.ToHeaders UpdateInput' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateInput' where
  toJSON UpdateInput'' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("inputDevices" Core..=) Prelude.<$> inputDevices,
            ("sources" Core..=) Prelude.<$> sources,
            ("inputSecurityGroups" Core..=)
              Prelude.<$> inputSecurityGroups,
            ("destinations" Core..=) Prelude.<$> destinations,
            ("name" Core..=) Prelude.<$> name,
            ("mediaConnectFlows" Core..=)
              Prelude.<$> mediaConnectFlows,
            ("roleArn" Core..=) Prelude.<$> roleArn
          ]
      )

instance Core.ToPath UpdateInput' where
  toPath UpdateInput'' {..} =
    Prelude.mconcat
      ["/prod/inputs/", Core.toBS inputId]

instance Core.ToQuery UpdateInput' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for UpdateInputResponse
--
-- /See:/ 'newUpdateInputResponse' smart constructor.
data UpdateInputResponse = UpdateInputResponse'
  { input :: Prelude.Maybe Input,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateInputResponse where
  rnf UpdateInputResponse' {..} =
    Prelude.rnf input
      `Prelude.seq` Prelude.rnf httpStatus
