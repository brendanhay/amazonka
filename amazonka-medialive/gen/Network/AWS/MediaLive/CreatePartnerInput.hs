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
-- Module      : Network.AWS.MediaLive.CreatePartnerInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a partner input
module Network.AWS.MediaLive.CreatePartnerInput
  ( -- * Creating a Request
    CreatePartnerInput' (..),
    newCreatePartnerInput',

    -- * Request Lenses
    createPartnerInput'_requestId,
    createPartnerInput'_tags,
    createPartnerInput'_inputId,

    -- * Destructuring the Response
    CreatePartnerInputResponse (..),
    newCreatePartnerInputResponse,

    -- * Response Lenses
    createPartnerInputResponse_input,
    createPartnerInputResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to create a partner input
--
-- /See:/ 'newCreatePartnerInput'' smart constructor.
data CreatePartnerInput' = CreatePartnerInput''
  { -- | Unique identifier of the request to ensure the request is handled
    -- exactly once in case of retries.
    requestId :: Core.Maybe Core.Text,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Unique ID of the input.
    inputId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePartnerInput'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'createPartnerInput'_requestId' - Unique identifier of the request to ensure the request is handled
-- exactly once in case of retries.
--
-- 'tags', 'createPartnerInput'_tags' - A collection of key-value pairs.
--
-- 'inputId', 'createPartnerInput'_inputId' - Unique ID of the input.
newCreatePartnerInput' ::
  -- | 'inputId'
  Core.Text ->
  CreatePartnerInput'
newCreatePartnerInput' pInputId_ =
  CreatePartnerInput''
    { requestId = Core.Nothing,
      tags = Core.Nothing,
      inputId = pInputId_
    }

-- | Unique identifier of the request to ensure the request is handled
-- exactly once in case of retries.
createPartnerInput'_requestId :: Lens.Lens' CreatePartnerInput' (Core.Maybe Core.Text)
createPartnerInput'_requestId = Lens.lens (\CreatePartnerInput'' {requestId} -> requestId) (\s@CreatePartnerInput'' {} a -> s {requestId = a} :: CreatePartnerInput')

-- | A collection of key-value pairs.
createPartnerInput'_tags :: Lens.Lens' CreatePartnerInput' (Core.Maybe (Core.HashMap Core.Text Core.Text))
createPartnerInput'_tags = Lens.lens (\CreatePartnerInput'' {tags} -> tags) (\s@CreatePartnerInput'' {} a -> s {tags = a} :: CreatePartnerInput') Core.. Lens.mapping Lens._Coerce

-- | Unique ID of the input.
createPartnerInput'_inputId :: Lens.Lens' CreatePartnerInput' Core.Text
createPartnerInput'_inputId = Lens.lens (\CreatePartnerInput'' {inputId} -> inputId) (\s@CreatePartnerInput'' {} a -> s {inputId = a} :: CreatePartnerInput')

instance Core.AWSRequest CreatePartnerInput' where
  type
    AWSResponse CreatePartnerInput' =
      CreatePartnerInputResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePartnerInputResponse'
            Core.<$> (x Core..?> "input")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePartnerInput'

instance Core.NFData CreatePartnerInput'

instance Core.ToHeaders CreatePartnerInput' where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePartnerInput' where
  toJSON CreatePartnerInput'' {..} =
    Core.object
      ( Core.catMaybes
          [ ("requestId" Core..=) Core.<$> requestId,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreatePartnerInput' where
  toPath CreatePartnerInput'' {..} =
    Core.mconcat
      ["/prod/inputs/", Core.toBS inputId, "/partners"]

instance Core.ToQuery CreatePartnerInput' where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for CreatePartnerInputResponse
--
-- /See:/ 'newCreatePartnerInputResponse' smart constructor.
data CreatePartnerInputResponse = CreatePartnerInputResponse'
  { input :: Core.Maybe Input,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePartnerInputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'createPartnerInputResponse_input' - Undocumented member.
--
-- 'httpStatus', 'createPartnerInputResponse_httpStatus' - The response's http status code.
newCreatePartnerInputResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePartnerInputResponse
newCreatePartnerInputResponse pHttpStatus_ =
  CreatePartnerInputResponse'
    { input = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createPartnerInputResponse_input :: Lens.Lens' CreatePartnerInputResponse (Core.Maybe Input)
createPartnerInputResponse_input = Lens.lens (\CreatePartnerInputResponse' {input} -> input) (\s@CreatePartnerInputResponse' {} a -> s {input = a} :: CreatePartnerInputResponse)

-- | The response's http status code.
createPartnerInputResponse_httpStatus :: Lens.Lens' CreatePartnerInputResponse Core.Int
createPartnerInputResponse_httpStatus = Lens.lens (\CreatePartnerInputResponse' {httpStatus} -> httpStatus) (\s@CreatePartnerInputResponse' {} a -> s {httpStatus = a} :: CreatePartnerInputResponse)

instance Core.NFData CreatePartnerInputResponse
