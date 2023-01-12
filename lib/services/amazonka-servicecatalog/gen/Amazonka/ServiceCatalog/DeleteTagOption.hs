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
-- Module      : Amazonka.ServiceCatalog.DeleteTagOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified TagOption.
--
-- You cannot delete a TagOption if it is associated with a product or
-- portfolio.
module Amazonka.ServiceCatalog.DeleteTagOption
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDeleteTagOption' smart constructor.
data DeleteTagOption = DeleteTagOption'
  { -- | The TagOption identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteTagOption
newDeleteTagOption pId_ = DeleteTagOption' {id = pId_}

-- | The TagOption identifier.
deleteTagOption_id :: Lens.Lens' DeleteTagOption Prelude.Text
deleteTagOption_id = Lens.lens (\DeleteTagOption' {id} -> id) (\s@DeleteTagOption' {} a -> s {id = a} :: DeleteTagOption)

instance Core.AWSRequest DeleteTagOption where
  type
    AWSResponse DeleteTagOption =
      DeleteTagOptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTagOptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTagOption where
  hashWithSalt _salt DeleteTagOption' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteTagOption where
  rnf DeleteTagOption' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteTagOption where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DeleteTagOption" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTagOption where
  toJSON DeleteTagOption' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("Id" Data..= id)])

instance Data.ToPath DeleteTagOption where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTagOption where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTagOptionResponse' smart constructor.
data DeleteTagOptionResponse = DeleteTagOptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteTagOptionResponse
newDeleteTagOptionResponse pHttpStatus_ =
  DeleteTagOptionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTagOptionResponse_httpStatus :: Lens.Lens' DeleteTagOptionResponse Prelude.Int
deleteTagOptionResponse_httpStatus = Lens.lens (\DeleteTagOptionResponse' {httpStatus} -> httpStatus) (\s@DeleteTagOptionResponse' {} a -> s {httpStatus = a} :: DeleteTagOptionResponse)

instance Prelude.NFData DeleteTagOptionResponse where
  rnf DeleteTagOptionResponse' {..} =
    Prelude.rnf httpStatus
