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
-- Module      : Amazonka.MwAA.CreateCliToken
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a CLI token to use Airflow CLI.
module Amazonka.MwAA.CreateCliToken
  ( -- * Creating a Request
    CreateCliToken (..),
    newCreateCliToken,

    -- * Request Lenses
    createCliToken_name,

    -- * Destructuring the Response
    CreateCliTokenResponse (..),
    newCreateCliTokenResponse,

    -- * Response Lenses
    createCliTokenResponse_webServerHostname,
    createCliTokenResponse_cliToken,
    createCliTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MwAA.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCliToken' smart constructor.
data CreateCliToken = CreateCliToken'
  { -- | Create a CLI token request for a MWAA environment.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCliToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createCliToken_name' - Create a CLI token request for a MWAA environment.
newCreateCliToken ::
  -- | 'name'
  Prelude.Text ->
  CreateCliToken
newCreateCliToken pName_ =
  CreateCliToken' {name = pName_}

-- | Create a CLI token request for a MWAA environment.
createCliToken_name :: Lens.Lens' CreateCliToken Prelude.Text
createCliToken_name = Lens.lens (\CreateCliToken' {name} -> name) (\s@CreateCliToken' {} a -> s {name = a} :: CreateCliToken)

instance Core.AWSRequest CreateCliToken where
  type
    AWSResponse CreateCliToken =
      CreateCliTokenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCliTokenResponse'
            Prelude.<$> (x Core..?> "WebServerHostname")
            Prelude.<*> (x Core..?> "CliToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCliToken where
  hashWithSalt salt' CreateCliToken' {..} =
    salt' `Prelude.hashWithSalt` name

instance Prelude.NFData CreateCliToken where
  rnf CreateCliToken' {..} = Prelude.rnf name

instance Core.ToHeaders CreateCliToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCliToken where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CreateCliToken where
  toPath CreateCliToken' {..} =
    Prelude.mconcat ["/clitoken/", Core.toBS name]

instance Core.ToQuery CreateCliToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCliTokenResponse' smart constructor.
data CreateCliTokenResponse = CreateCliTokenResponse'
  { -- | Create an Airflow CLI login token response for the provided webserver
    -- hostname.
    webServerHostname :: Prelude.Maybe Prelude.Text,
    -- | Create an Airflow CLI login token response for the provided JWT token.
    cliToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCliTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webServerHostname', 'createCliTokenResponse_webServerHostname' - Create an Airflow CLI login token response for the provided webserver
-- hostname.
--
-- 'cliToken', 'createCliTokenResponse_cliToken' - Create an Airflow CLI login token response for the provided JWT token.
--
-- 'httpStatus', 'createCliTokenResponse_httpStatus' - The response's http status code.
newCreateCliTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCliTokenResponse
newCreateCliTokenResponse pHttpStatus_ =
  CreateCliTokenResponse'
    { webServerHostname =
        Prelude.Nothing,
      cliToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Create an Airflow CLI login token response for the provided webserver
-- hostname.
createCliTokenResponse_webServerHostname :: Lens.Lens' CreateCliTokenResponse (Prelude.Maybe Prelude.Text)
createCliTokenResponse_webServerHostname = Lens.lens (\CreateCliTokenResponse' {webServerHostname} -> webServerHostname) (\s@CreateCliTokenResponse' {} a -> s {webServerHostname = a} :: CreateCliTokenResponse)

-- | Create an Airflow CLI login token response for the provided JWT token.
createCliTokenResponse_cliToken :: Lens.Lens' CreateCliTokenResponse (Prelude.Maybe Prelude.Text)
createCliTokenResponse_cliToken = Lens.lens (\CreateCliTokenResponse' {cliToken} -> cliToken) (\s@CreateCliTokenResponse' {} a -> s {cliToken = a} :: CreateCliTokenResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
createCliTokenResponse_httpStatus :: Lens.Lens' CreateCliTokenResponse Prelude.Int
createCliTokenResponse_httpStatus = Lens.lens (\CreateCliTokenResponse' {httpStatus} -> httpStatus) (\s@CreateCliTokenResponse' {} a -> s {httpStatus = a} :: CreateCliTokenResponse)

instance Prelude.NFData CreateCliTokenResponse where
  rnf CreateCliTokenResponse' {..} =
    Prelude.rnf webServerHostname
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf cliToken
