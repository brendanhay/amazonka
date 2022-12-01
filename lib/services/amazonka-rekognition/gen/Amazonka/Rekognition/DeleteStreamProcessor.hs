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
-- Module      : Amazonka.Rekognition.DeleteStreamProcessor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the stream processor identified by @Name@. You assign the value
-- for @Name@ when you create the stream processor with
-- CreateStreamProcessor. You might not be able to use the same name for a
-- stream processor for a few seconds after calling
-- @DeleteStreamProcessor@.
module Amazonka.Rekognition.DeleteStreamProcessor
  ( -- * Creating a Request
    DeleteStreamProcessor (..),
    newDeleteStreamProcessor,

    -- * Request Lenses
    deleteStreamProcessor_name,

    -- * Destructuring the Response
    DeleteStreamProcessorResponse (..),
    newDeleteStreamProcessorResponse,

    -- * Response Lenses
    deleteStreamProcessorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStreamProcessor' smart constructor.
data DeleteStreamProcessor = DeleteStreamProcessor'
  { -- | The name of the stream processor you want to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteStreamProcessor_name' - The name of the stream processor you want to delete.
newDeleteStreamProcessor ::
  -- | 'name'
  Prelude.Text ->
  DeleteStreamProcessor
newDeleteStreamProcessor pName_ =
  DeleteStreamProcessor' {name = pName_}

-- | The name of the stream processor you want to delete.
deleteStreamProcessor_name :: Lens.Lens' DeleteStreamProcessor Prelude.Text
deleteStreamProcessor_name = Lens.lens (\DeleteStreamProcessor' {name} -> name) (\s@DeleteStreamProcessor' {} a -> s {name = a} :: DeleteStreamProcessor)

instance Core.AWSRequest DeleteStreamProcessor where
  type
    AWSResponse DeleteStreamProcessor =
      DeleteStreamProcessorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStreamProcessorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStreamProcessor where
  hashWithSalt _salt DeleteStreamProcessor' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteStreamProcessor where
  rnf DeleteStreamProcessor' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteStreamProcessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DeleteStreamProcessor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteStreamProcessor where
  toJSON DeleteStreamProcessor' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath DeleteStreamProcessor where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteStreamProcessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStreamProcessorResponse' smart constructor.
data DeleteStreamProcessorResponse = DeleteStreamProcessorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamProcessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStreamProcessorResponse_httpStatus' - The response's http status code.
newDeleteStreamProcessorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStreamProcessorResponse
newDeleteStreamProcessorResponse pHttpStatus_ =
  DeleteStreamProcessorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteStreamProcessorResponse_httpStatus :: Lens.Lens' DeleteStreamProcessorResponse Prelude.Int
deleteStreamProcessorResponse_httpStatus = Lens.lens (\DeleteStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@DeleteStreamProcessorResponse' {} a -> s {httpStatus = a} :: DeleteStreamProcessorResponse)

instance Prelude.NFData DeleteStreamProcessorResponse where
  rnf DeleteStreamProcessorResponse' {..} =
    Prelude.rnf httpStatus
