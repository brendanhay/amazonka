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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a CLI token for the Airflow CLI. To learn more, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/call-mwaa-apis-cli.html Creating an Apache Airflow CLI token>.
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MwAA.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCliToken' smart constructor.
data CreateCliToken = CreateCliToken'
  { -- | The name of the Amazon MWAA environment. For example,
    -- @MyMWAAEnvironment@.
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
-- 'name', 'createCliToken_name' - The name of the Amazon MWAA environment. For example,
-- @MyMWAAEnvironment@.
newCreateCliToken ::
  -- | 'name'
  Prelude.Text ->
  CreateCliToken
newCreateCliToken pName_ =
  CreateCliToken' {name = pName_}

-- | The name of the Amazon MWAA environment. For example,
-- @MyMWAAEnvironment@.
createCliToken_name :: Lens.Lens' CreateCliToken Prelude.Text
createCliToken_name = Lens.lens (\CreateCliToken' {name} -> name) (\s@CreateCliToken' {} a -> s {name = a} :: CreateCliToken)

instance Core.AWSRequest CreateCliToken where
  type
    AWSResponse CreateCliToken =
      CreateCliTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCliTokenResponse'
            Prelude.<$> (x Data..?> "WebServerHostname")
            Prelude.<*> (x Data..?> "CliToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCliToken where
  hashWithSalt _salt CreateCliToken' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData CreateCliToken where
  rnf CreateCliToken' {..} = Prelude.rnf name

instance Data.ToHeaders CreateCliToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCliToken where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CreateCliToken where
  toPath CreateCliToken' {..} =
    Prelude.mconcat ["/clitoken/", Data.toBS name]

instance Data.ToQuery CreateCliToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCliTokenResponse' smart constructor.
data CreateCliTokenResponse = CreateCliTokenResponse'
  { -- | The Airflow web server hostname for the environment.
    webServerHostname :: Prelude.Maybe Prelude.Text,
    -- | An Airflow CLI login token.
    cliToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
-- 'webServerHostname', 'createCliTokenResponse_webServerHostname' - The Airflow web server hostname for the environment.
--
-- 'cliToken', 'createCliTokenResponse_cliToken' - An Airflow CLI login token.
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

-- | The Airflow web server hostname for the environment.
createCliTokenResponse_webServerHostname :: Lens.Lens' CreateCliTokenResponse (Prelude.Maybe Prelude.Text)
createCliTokenResponse_webServerHostname = Lens.lens (\CreateCliTokenResponse' {webServerHostname} -> webServerHostname) (\s@CreateCliTokenResponse' {} a -> s {webServerHostname = a} :: CreateCliTokenResponse)

-- | An Airflow CLI login token.
createCliTokenResponse_cliToken :: Lens.Lens' CreateCliTokenResponse (Prelude.Maybe Prelude.Text)
createCliTokenResponse_cliToken = Lens.lens (\CreateCliTokenResponse' {cliToken} -> cliToken) (\s@CreateCliTokenResponse' {} a -> s {cliToken = a} :: CreateCliTokenResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
createCliTokenResponse_httpStatus :: Lens.Lens' CreateCliTokenResponse Prelude.Int
createCliTokenResponse_httpStatus = Lens.lens (\CreateCliTokenResponse' {httpStatus} -> httpStatus) (\s@CreateCliTokenResponse' {} a -> s {httpStatus = a} :: CreateCliTokenResponse)

instance Prelude.NFData CreateCliTokenResponse where
  rnf CreateCliTokenResponse' {..} =
    Prelude.rnf webServerHostname
      `Prelude.seq` Prelude.rnf cliToken
      `Prelude.seq` Prelude.rnf httpStatus
