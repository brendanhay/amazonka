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
-- Module      : Amazonka.MwAA.CreateWebLoginToken
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a web login token for the Airflow Web UI. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/call-mwaa-apis-web.html Creating an Apache Airflow web login token>.
module Amazonka.MwAA.CreateWebLoginToken
  ( -- * Creating a Request
    CreateWebLoginToken (..),
    newCreateWebLoginToken,

    -- * Request Lenses
    createWebLoginToken_name,

    -- * Destructuring the Response
    CreateWebLoginTokenResponse (..),
    newCreateWebLoginTokenResponse,

    -- * Response Lenses
    createWebLoginTokenResponse_webServerHostname,
    createWebLoginTokenResponse_webToken,
    createWebLoginTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MwAA.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWebLoginToken' smart constructor.
data CreateWebLoginToken = CreateWebLoginToken'
  { -- | The name of the Amazon MWAA environment. For example,
    -- @MyMWAAEnvironment@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWebLoginToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createWebLoginToken_name' - The name of the Amazon MWAA environment. For example,
-- @MyMWAAEnvironment@.
newCreateWebLoginToken ::
  -- | 'name'
  Prelude.Text ->
  CreateWebLoginToken
newCreateWebLoginToken pName_ =
  CreateWebLoginToken' {name = pName_}

-- | The name of the Amazon MWAA environment. For example,
-- @MyMWAAEnvironment@.
createWebLoginToken_name :: Lens.Lens' CreateWebLoginToken Prelude.Text
createWebLoginToken_name = Lens.lens (\CreateWebLoginToken' {name} -> name) (\s@CreateWebLoginToken' {} a -> s {name = a} :: CreateWebLoginToken)

instance Core.AWSRequest CreateWebLoginToken where
  type
    AWSResponse CreateWebLoginToken =
      CreateWebLoginTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWebLoginTokenResponse'
            Prelude.<$> (x Data..?> "WebServerHostname")
            Prelude.<*> (x Data..?> "WebToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWebLoginToken where
  hashWithSalt _salt CreateWebLoginToken' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData CreateWebLoginToken where
  rnf CreateWebLoginToken' {..} = Prelude.rnf name

instance Data.ToHeaders CreateWebLoginToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWebLoginToken where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CreateWebLoginToken where
  toPath CreateWebLoginToken' {..} =
    Prelude.mconcat ["/webtoken/", Data.toBS name]

instance Data.ToQuery CreateWebLoginToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWebLoginTokenResponse' smart constructor.
data CreateWebLoginTokenResponse = CreateWebLoginTokenResponse'
  { -- | The Airflow web server hostname for the environment.
    webServerHostname :: Prelude.Maybe Prelude.Text,
    -- | An Airflow web server login token.
    webToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWebLoginTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webServerHostname', 'createWebLoginTokenResponse_webServerHostname' - The Airflow web server hostname for the environment.
--
-- 'webToken', 'createWebLoginTokenResponse_webToken' - An Airflow web server login token.
--
-- 'httpStatus', 'createWebLoginTokenResponse_httpStatus' - The response's http status code.
newCreateWebLoginTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWebLoginTokenResponse
newCreateWebLoginTokenResponse pHttpStatus_ =
  CreateWebLoginTokenResponse'
    { webServerHostname =
        Prelude.Nothing,
      webToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Airflow web server hostname for the environment.
createWebLoginTokenResponse_webServerHostname :: Lens.Lens' CreateWebLoginTokenResponse (Prelude.Maybe Prelude.Text)
createWebLoginTokenResponse_webServerHostname = Lens.lens (\CreateWebLoginTokenResponse' {webServerHostname} -> webServerHostname) (\s@CreateWebLoginTokenResponse' {} a -> s {webServerHostname = a} :: CreateWebLoginTokenResponse)

-- | An Airflow web server login token.
createWebLoginTokenResponse_webToken :: Lens.Lens' CreateWebLoginTokenResponse (Prelude.Maybe Prelude.Text)
createWebLoginTokenResponse_webToken = Lens.lens (\CreateWebLoginTokenResponse' {webToken} -> webToken) (\s@CreateWebLoginTokenResponse' {} a -> s {webToken = a} :: CreateWebLoginTokenResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
createWebLoginTokenResponse_httpStatus :: Lens.Lens' CreateWebLoginTokenResponse Prelude.Int
createWebLoginTokenResponse_httpStatus = Lens.lens (\CreateWebLoginTokenResponse' {httpStatus} -> httpStatus) (\s@CreateWebLoginTokenResponse' {} a -> s {httpStatus = a} :: CreateWebLoginTokenResponse)

instance Prelude.NFData CreateWebLoginTokenResponse where
  rnf CreateWebLoginTokenResponse' {..} =
    Prelude.rnf webServerHostname
      `Prelude.seq` Prelude.rnf webToken
      `Prelude.seq` Prelude.rnf httpStatus
