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
-- Module      : Network.AWS.ServiceCatalog.DeleteTagOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified TagOption.
--
-- You cannot delete a TagOption if it is associated with a product or
-- portfolio.
module Network.AWS.ServiceCatalog.DeleteTagOption
  ( -- * Creating a Request
    DeleteTagOption (..),
    newDeleteTagOption,

    -- * Request Lenses
    deleteTagOption_id,

    -- * Destructuring the Response
    DeleteTagOptionResponse (..),
    newDeleteTagOptionResponse,

    -- * Response Lenses
    deleteTagOptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDeleteTagOption' smart constructor.
data DeleteTagOption = DeleteTagOption'
  { -- | The TagOption identifier.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTagOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteTagOption_id' - The TagOption identifier.
newDeleteTagOption ::
  -- | 'id'
  Core.Text ->
  DeleteTagOption
newDeleteTagOption pId_ = DeleteTagOption' {id = pId_}

-- | The TagOption identifier.
deleteTagOption_id :: Lens.Lens' DeleteTagOption Core.Text
deleteTagOption_id = Lens.lens (\DeleteTagOption' {id} -> id) (\s@DeleteTagOption' {} a -> s {id = a} :: DeleteTagOption)

instance Core.AWSRequest DeleteTagOption where
  type
    AWSResponse DeleteTagOption =
      DeleteTagOptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTagOptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTagOption

instance Core.NFData DeleteTagOption

instance Core.ToHeaders DeleteTagOption where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DeleteTagOption" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteTagOption where
  toJSON DeleteTagOption' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.ToPath DeleteTagOption where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTagOption where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteTagOptionResponse' smart constructor.
data DeleteTagOptionResponse = DeleteTagOptionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTagOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTagOptionResponse_httpStatus' - The response's http status code.
newDeleteTagOptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTagOptionResponse
newDeleteTagOptionResponse pHttpStatus_ =
  DeleteTagOptionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTagOptionResponse_httpStatus :: Lens.Lens' DeleteTagOptionResponse Core.Int
deleteTagOptionResponse_httpStatus = Lens.lens (\DeleteTagOptionResponse' {httpStatus} -> httpStatus) (\s@DeleteTagOptionResponse' {} a -> s {httpStatus = a} :: DeleteTagOptionResponse)

instance Core.NFData DeleteTagOptionResponse
