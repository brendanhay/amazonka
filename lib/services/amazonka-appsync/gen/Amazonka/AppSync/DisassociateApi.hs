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
-- Module      : Amazonka.AppSync.DisassociateApi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an @ApiAssociation@ object from a custom domain.
module Amazonka.AppSync.DisassociateApi
  ( -- * Creating a Request
    DisassociateApi (..),
    newDisassociateApi,

    -- * Request Lenses
    disassociateApi_domainName,

    -- * Destructuring the Response
    DisassociateApiResponse (..),
    newDisassociateApiResponse,

    -- * Response Lenses
    disassociateApiResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateApi' smart constructor.
data DisassociateApi = DisassociateApi'
  { -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'disassociateApi_domainName' - The domain name.
newDisassociateApi ::
  -- | 'domainName'
  Prelude.Text ->
  DisassociateApi
newDisassociateApi pDomainName_ =
  DisassociateApi' {domainName = pDomainName_}

-- | The domain name.
disassociateApi_domainName :: Lens.Lens' DisassociateApi Prelude.Text
disassociateApi_domainName = Lens.lens (\DisassociateApi' {domainName} -> domainName) (\s@DisassociateApi' {} a -> s {domainName = a} :: DisassociateApi)

instance Core.AWSRequest DisassociateApi where
  type
    AWSResponse DisassociateApi =
      DisassociateApiResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateApiResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateApi where
  hashWithSalt _salt DisassociateApi' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DisassociateApi where
  rnf DisassociateApi' {..} = Prelude.rnf domainName

instance Data.ToHeaders DisassociateApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateApi where
  toPath DisassociateApi' {..} =
    Prelude.mconcat
      [ "/v1/domainnames/",
        Data.toBS domainName,
        "/apiassociation"
      ]

instance Data.ToQuery DisassociateApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateApiResponse' smart constructor.
data DisassociateApiResponse = DisassociateApiResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateApiResponse_httpStatus' - The response's http status code.
newDisassociateApiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateApiResponse
newDisassociateApiResponse pHttpStatus_ =
  DisassociateApiResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
disassociateApiResponse_httpStatus :: Lens.Lens' DisassociateApiResponse Prelude.Int
disassociateApiResponse_httpStatus = Lens.lens (\DisassociateApiResponse' {httpStatus} -> httpStatus) (\s@DisassociateApiResponse' {} a -> s {httpStatus = a} :: DisassociateApiResponse)

instance Prelude.NFData DisassociateApiResponse where
  rnf DisassociateApiResponse' {..} =
    Prelude.rnf httpStatus
