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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a JWT token to be used to login to Airflow Web UI with claims
-- based Authentication.
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
import qualified Amazonka.Lens as Lens
import Amazonka.MwAA.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWebLoginToken' smart constructor.
data CreateWebLoginToken = CreateWebLoginToken'
  { -- | Create an Airflow Web UI login token request for a MWAA environment.
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
-- 'name', 'createWebLoginToken_name' - Create an Airflow Web UI login token request for a MWAA environment.
newCreateWebLoginToken ::
  -- | 'name'
  Prelude.Text ->
  CreateWebLoginToken
newCreateWebLoginToken pName_ =
  CreateWebLoginToken' {name = pName_}

-- | Create an Airflow Web UI login token request for a MWAA environment.
createWebLoginToken_name :: Lens.Lens' CreateWebLoginToken Prelude.Text
createWebLoginToken_name = Lens.lens (\CreateWebLoginToken' {name} -> name) (\s@CreateWebLoginToken' {} a -> s {name = a} :: CreateWebLoginToken)

instance Core.AWSRequest CreateWebLoginToken where
  type
    AWSResponse CreateWebLoginToken =
      CreateWebLoginTokenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWebLoginTokenResponse'
            Prelude.<$> (x Core..?> "WebServerHostname")
            Prelude.<*> (x Core..?> "WebToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWebLoginToken where
  hashWithSalt salt' CreateWebLoginToken' {..} =
    salt' `Prelude.hashWithSalt` name

instance Prelude.NFData CreateWebLoginToken where
  rnf CreateWebLoginToken' {..} = Prelude.rnf name

instance Core.ToHeaders CreateWebLoginToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWebLoginToken where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CreateWebLoginToken where
  toPath CreateWebLoginToken' {..} =
    Prelude.mconcat ["/webtoken/", Core.toBS name]

instance Core.ToQuery CreateWebLoginToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWebLoginTokenResponse' smart constructor.
data CreateWebLoginTokenResponse = CreateWebLoginTokenResponse'
  { -- | Create an Airflow Web UI login token response for the provided webserver
    -- hostname.
    webServerHostname :: Prelude.Maybe Prelude.Text,
    -- | Create an Airflow Web UI login token response for the provided JWT
    -- token.
    webToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
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
-- 'webServerHostname', 'createWebLoginTokenResponse_webServerHostname' - Create an Airflow Web UI login token response for the provided webserver
-- hostname.
--
-- 'webToken', 'createWebLoginTokenResponse_webToken' - Create an Airflow Web UI login token response for the provided JWT
-- token.
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

-- | Create an Airflow Web UI login token response for the provided webserver
-- hostname.
createWebLoginTokenResponse_webServerHostname :: Lens.Lens' CreateWebLoginTokenResponse (Prelude.Maybe Prelude.Text)
createWebLoginTokenResponse_webServerHostname = Lens.lens (\CreateWebLoginTokenResponse' {webServerHostname} -> webServerHostname) (\s@CreateWebLoginTokenResponse' {} a -> s {webServerHostname = a} :: CreateWebLoginTokenResponse)

-- | Create an Airflow Web UI login token response for the provided JWT
-- token.
createWebLoginTokenResponse_webToken :: Lens.Lens' CreateWebLoginTokenResponse (Prelude.Maybe Prelude.Text)
createWebLoginTokenResponse_webToken = Lens.lens (\CreateWebLoginTokenResponse' {webToken} -> webToken) (\s@CreateWebLoginTokenResponse' {} a -> s {webToken = a} :: CreateWebLoginTokenResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
createWebLoginTokenResponse_httpStatus :: Lens.Lens' CreateWebLoginTokenResponse Prelude.Int
createWebLoginTokenResponse_httpStatus = Lens.lens (\CreateWebLoginTokenResponse' {httpStatus} -> httpStatus) (\s@CreateWebLoginTokenResponse' {} a -> s {httpStatus = a} :: CreateWebLoginTokenResponse)

instance Prelude.NFData CreateWebLoginTokenResponse where
  rnf CreateWebLoginTokenResponse' {..} =
    Prelude.rnf webServerHostname
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf webToken
