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
-- Module      : Amazonka.AppSync.GetDomainName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a custom @DomainName@ object.
module Amazonka.AppSync.GetDomainName
  ( -- * Creating a Request
    GetDomainName (..),
    newGetDomainName,

    -- * Request Lenses
    getDomainName_domainName,

    -- * Destructuring the Response
    GetDomainNameResponse (..),
    newGetDomainNameResponse,

    -- * Response Lenses
    getDomainNameResponse_domainNameConfig,
    getDomainNameResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDomainName' smart constructor.
data GetDomainName = GetDomainName'
  { -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getDomainName_domainName' - The domain name.
newGetDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  GetDomainName
newGetDomainName pDomainName_ =
  GetDomainName' {domainName = pDomainName_}

-- | The domain name.
getDomainName_domainName :: Lens.Lens' GetDomainName Prelude.Text
getDomainName_domainName = Lens.lens (\GetDomainName' {domainName} -> domainName) (\s@GetDomainName' {} a -> s {domainName = a} :: GetDomainName)

instance Core.AWSRequest GetDomainName where
  type
    AWSResponse GetDomainName =
      GetDomainNameResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainNameResponse'
            Prelude.<$> (x Data..?> "domainNameConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDomainName where
  hashWithSalt _salt GetDomainName' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetDomainName where
  rnf GetDomainName' {..} = Prelude.rnf domainName

instance Data.ToHeaders GetDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDomainName where
  toPath GetDomainName' {..} =
    Prelude.mconcat
      ["/v1/domainnames/", Data.toBS domainName]

instance Data.ToQuery GetDomainName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDomainNameResponse' smart constructor.
data GetDomainNameResponse = GetDomainNameResponse'
  { -- | The configuration for the @DomainName@.
    domainNameConfig :: Prelude.Maybe DomainNameConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNameConfig', 'getDomainNameResponse_domainNameConfig' - The configuration for the @DomainName@.
--
-- 'httpStatus', 'getDomainNameResponse_httpStatus' - The response's http status code.
newGetDomainNameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDomainNameResponse
newGetDomainNameResponse pHttpStatus_ =
  GetDomainNameResponse'
    { domainNameConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration for the @DomainName@.
getDomainNameResponse_domainNameConfig :: Lens.Lens' GetDomainNameResponse (Prelude.Maybe DomainNameConfig)
getDomainNameResponse_domainNameConfig = Lens.lens (\GetDomainNameResponse' {domainNameConfig} -> domainNameConfig) (\s@GetDomainNameResponse' {} a -> s {domainNameConfig = a} :: GetDomainNameResponse)

-- | The response's http status code.
getDomainNameResponse_httpStatus :: Lens.Lens' GetDomainNameResponse Prelude.Int
getDomainNameResponse_httpStatus = Lens.lens (\GetDomainNameResponse' {httpStatus} -> httpStatus) (\s@GetDomainNameResponse' {} a -> s {httpStatus = a} :: GetDomainNameResponse)

instance Prelude.NFData GetDomainNameResponse where
  rnf GetDomainNameResponse' {..} =
    Prelude.rnf domainNameConfig `Prelude.seq`
      Prelude.rnf httpStatus
