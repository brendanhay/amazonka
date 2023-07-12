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
-- Module      : Amazonka.Shield.DeleteProtection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Shield Advanced Protection.
module Amazonka.Shield.DeleteProtection
  ( -- * Creating a Request
    DeleteProtection (..),
    newDeleteProtection,

    -- * Request Lenses
    deleteProtection_protectionId,

    -- * Destructuring the Response
    DeleteProtectionResponse (..),
    newDeleteProtectionResponse,

    -- * Response Lenses
    deleteProtectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newDeleteProtection' smart constructor.
data DeleteProtection = DeleteProtection'
  { -- | The unique identifier (ID) for the Protection object to be deleted.
    protectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionId', 'deleteProtection_protectionId' - The unique identifier (ID) for the Protection object to be deleted.
newDeleteProtection ::
  -- | 'protectionId'
  Prelude.Text ->
  DeleteProtection
newDeleteProtection pProtectionId_ =
  DeleteProtection' {protectionId = pProtectionId_}

-- | The unique identifier (ID) for the Protection object to be deleted.
deleteProtection_protectionId :: Lens.Lens' DeleteProtection Prelude.Text
deleteProtection_protectionId = Lens.lens (\DeleteProtection' {protectionId} -> protectionId) (\s@DeleteProtection' {} a -> s {protectionId = a} :: DeleteProtection)

instance Core.AWSRequest DeleteProtection where
  type
    AWSResponse DeleteProtection =
      DeleteProtectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProtectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProtection where
  hashWithSalt _salt DeleteProtection' {..} =
    _salt `Prelude.hashWithSalt` protectionId

instance Prelude.NFData DeleteProtection where
  rnf DeleteProtection' {..} = Prelude.rnf protectionId

instance Data.ToHeaders DeleteProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.DeleteProtection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteProtection where
  toJSON DeleteProtection' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProtectionId" Data..= protectionId)]
      )

instance Data.ToPath DeleteProtection where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProtectionResponse' smart constructor.
data DeleteProtectionResponse = DeleteProtectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProtectionResponse_httpStatus' - The response's http status code.
newDeleteProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProtectionResponse
newDeleteProtectionResponse pHttpStatus_ =
  DeleteProtectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProtectionResponse_httpStatus :: Lens.Lens' DeleteProtectionResponse Prelude.Int
deleteProtectionResponse_httpStatus = Lens.lens (\DeleteProtectionResponse' {httpStatus} -> httpStatus) (\s@DeleteProtectionResponse' {} a -> s {httpStatus = a} :: DeleteProtectionResponse)

instance Prelude.NFData DeleteProtectionResponse where
  rnf DeleteProtectionResponse' {..} =
    Prelude.rnf httpStatus
