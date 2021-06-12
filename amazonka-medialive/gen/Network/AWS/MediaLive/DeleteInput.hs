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
-- Module      : Network.AWS.MediaLive.DeleteInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the input end point
module Network.AWS.MediaLive.DeleteInput
  ( -- * Creating a Request
    DeleteInput (..),
    newDeleteInput,

    -- * Request Lenses
    deleteInput_inputId,

    -- * Destructuring the Response
    DeleteInputResponse (..),
    newDeleteInputResponse,

    -- * Response Lenses
    deleteInputResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteInputRequest
--
-- /See:/ 'newDeleteInput' smart constructor.
data DeleteInput = DeleteInput'
  { -- | Unique ID of the input
    inputId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputId', 'deleteInput_inputId' - Unique ID of the input
newDeleteInput ::
  -- | 'inputId'
  Core.Text ->
  DeleteInput
newDeleteInput pInputId_ =
  DeleteInput' {inputId = pInputId_}

-- | Unique ID of the input
deleteInput_inputId :: Lens.Lens' DeleteInput Core.Text
deleteInput_inputId = Lens.lens (\DeleteInput' {inputId} -> inputId) (\s@DeleteInput' {} a -> s {inputId = a} :: DeleteInput)

instance Core.AWSRequest DeleteInput where
  type AWSResponse DeleteInput = DeleteInputResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteInputResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteInput

instance Core.NFData DeleteInput

instance Core.ToHeaders DeleteInput where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteInput where
  toPath DeleteInput' {..} =
    Core.mconcat ["/prod/inputs/", Core.toBS inputId]

instance Core.ToQuery DeleteInput where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DeleteInputResponse
--
-- /See:/ 'newDeleteInputResponse' smart constructor.
data DeleteInputResponse = DeleteInputResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteInputResponse_httpStatus' - The response's http status code.
newDeleteInputResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteInputResponse
newDeleteInputResponse pHttpStatus_ =
  DeleteInputResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteInputResponse_httpStatus :: Lens.Lens' DeleteInputResponse Core.Int
deleteInputResponse_httpStatus = Lens.lens (\DeleteInputResponse' {httpStatus} -> httpStatus) (\s@DeleteInputResponse' {} a -> s {httpStatus = a} :: DeleteInputResponse)

instance Core.NFData DeleteInputResponse
