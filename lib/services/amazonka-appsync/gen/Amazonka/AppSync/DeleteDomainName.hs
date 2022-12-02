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
-- Module      : Amazonka.AppSync.DeleteDomainName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom @DomainName@ object.
module Amazonka.AppSync.DeleteDomainName
  ( -- * Creating a Request
    DeleteDomainName (..),
    newDeleteDomainName,

    -- * Request Lenses
    deleteDomainName_domainName,

    -- * Destructuring the Response
    DeleteDomainNameResponse (..),
    newDeleteDomainNameResponse,

    -- * Response Lenses
    deleteDomainNameResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDomainName' smart constructor.
data DeleteDomainName = DeleteDomainName'
  { -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteDomainName_domainName' - The domain name.
newDeleteDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  DeleteDomainName
newDeleteDomainName pDomainName_ =
  DeleteDomainName' {domainName = pDomainName_}

-- | The domain name.
deleteDomainName_domainName :: Lens.Lens' DeleteDomainName Prelude.Text
deleteDomainName_domainName = Lens.lens (\DeleteDomainName' {domainName} -> domainName) (\s@DeleteDomainName' {} a -> s {domainName = a} :: DeleteDomainName)

instance Core.AWSRequest DeleteDomainName where
  type
    AWSResponse DeleteDomainName =
      DeleteDomainNameResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDomainNameResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDomainName where
  hashWithSalt _salt DeleteDomainName' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DeleteDomainName where
  rnf DeleteDomainName' {..} = Prelude.rnf domainName

instance Data.ToHeaders DeleteDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDomainName where
  toPath DeleteDomainName' {..} =
    Prelude.mconcat
      ["/v1/domainnames/", Data.toBS domainName]

instance Data.ToQuery DeleteDomainName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDomainNameResponse' smart constructor.
data DeleteDomainNameResponse = DeleteDomainNameResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDomainNameResponse_httpStatus' - The response's http status code.
newDeleteDomainNameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDomainNameResponse
newDeleteDomainNameResponse pHttpStatus_ =
  DeleteDomainNameResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDomainNameResponse_httpStatus :: Lens.Lens' DeleteDomainNameResponse Prelude.Int
deleteDomainNameResponse_httpStatus = Lens.lens (\DeleteDomainNameResponse' {httpStatus} -> httpStatus) (\s@DeleteDomainNameResponse' {} a -> s {httpStatus = a} :: DeleteDomainNameResponse)

instance Prelude.NFData DeleteDomainNameResponse where
  rnf DeleteDomainNameResponse' {..} =
    Prelude.rnf httpStatus
