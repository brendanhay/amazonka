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
-- Module      : Amazonka.MacieV2.DeleteCustomDataIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Soft deletes a custom data identifier.
module Amazonka.MacieV2.DeleteCustomDataIdentifier
  ( -- * Creating a Request
    DeleteCustomDataIdentifier (..),
    newDeleteCustomDataIdentifier,

    -- * Request Lenses
    deleteCustomDataIdentifier_id,

    -- * Destructuring the Response
    DeleteCustomDataIdentifierResponse (..),
    newDeleteCustomDataIdentifierResponse,

    -- * Response Lenses
    deleteCustomDataIdentifierResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomDataIdentifier' smart constructor.
data DeleteCustomDataIdentifier = DeleteCustomDataIdentifier'
  { -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomDataIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteCustomDataIdentifier_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newDeleteCustomDataIdentifier ::
  -- | 'id'
  Prelude.Text ->
  DeleteCustomDataIdentifier
newDeleteCustomDataIdentifier pId_ =
  DeleteCustomDataIdentifier' {id = pId_}

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
deleteCustomDataIdentifier_id :: Lens.Lens' DeleteCustomDataIdentifier Prelude.Text
deleteCustomDataIdentifier_id = Lens.lens (\DeleteCustomDataIdentifier' {id} -> id) (\s@DeleteCustomDataIdentifier' {} a -> s {id = a} :: DeleteCustomDataIdentifier)

instance Core.AWSRequest DeleteCustomDataIdentifier where
  type
    AWSResponse DeleteCustomDataIdentifier =
      DeleteCustomDataIdentifierResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomDataIdentifierResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomDataIdentifier where
  hashWithSalt _salt DeleteCustomDataIdentifier' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteCustomDataIdentifier where
  rnf DeleteCustomDataIdentifier' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteCustomDataIdentifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCustomDataIdentifier where
  toPath DeleteCustomDataIdentifier' {..} =
    Prelude.mconcat
      ["/custom-data-identifiers/", Data.toBS id]

instance Data.ToQuery DeleteCustomDataIdentifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomDataIdentifierResponse' smart constructor.
data DeleteCustomDataIdentifierResponse = DeleteCustomDataIdentifierResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomDataIdentifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCustomDataIdentifierResponse_httpStatus' - The response's http status code.
newDeleteCustomDataIdentifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCustomDataIdentifierResponse
newDeleteCustomDataIdentifierResponse pHttpStatus_ =
  DeleteCustomDataIdentifierResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCustomDataIdentifierResponse_httpStatus :: Lens.Lens' DeleteCustomDataIdentifierResponse Prelude.Int
deleteCustomDataIdentifierResponse_httpStatus = Lens.lens (\DeleteCustomDataIdentifierResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomDataIdentifierResponse' {} a -> s {httpStatus = a} :: DeleteCustomDataIdentifierResponse)

instance
  Prelude.NFData
    DeleteCustomDataIdentifierResponse
  where
  rnf DeleteCustomDataIdentifierResponse' {..} =
    Prelude.rnf httpStatus
