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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteInputRequest
--
-- /See:/ 'newDeleteInput' smart constructor.
data DeleteInput = DeleteInput'
  { -- | Unique ID of the input
    inputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteInput
newDeleteInput pInputId_ =
  DeleteInput' {inputId = pInputId_}

-- | Unique ID of the input
deleteInput_inputId :: Lens.Lens' DeleteInput Prelude.Text
deleteInput_inputId = Lens.lens (\DeleteInput' {inputId} -> inputId) (\s@DeleteInput' {} a -> s {inputId = a} :: DeleteInput)

instance Prelude.AWSRequest DeleteInput where
  type Rs DeleteInput = DeleteInputResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteInputResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInput

instance Prelude.NFData DeleteInput

instance Prelude.ToHeaders DeleteInput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteInput where
  toPath DeleteInput' {..} =
    Prelude.mconcat
      ["/prod/inputs/", Prelude.toBS inputId]

instance Prelude.ToQuery DeleteInput where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DeleteInputResponse
--
-- /See:/ 'newDeleteInputResponse' smart constructor.
data DeleteInputResponse = DeleteInputResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteInputResponse
newDeleteInputResponse pHttpStatus_ =
  DeleteInputResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteInputResponse_httpStatus :: Lens.Lens' DeleteInputResponse Prelude.Int
deleteInputResponse_httpStatus = Lens.lens (\DeleteInputResponse' {httpStatus} -> httpStatus) (\s@DeleteInputResponse' {} a -> s {httpStatus = a} :: DeleteInputResponse)

instance Prelude.NFData DeleteInputResponse
