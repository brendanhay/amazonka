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
-- Module      : Amazonka.CognitoIdentityProvider.GetCSVHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the header information for the comma-separated value (CSV) file to
-- be used as input for the user import job.
module Amazonka.CognitoIdentityProvider.GetCSVHeader
  ( -- * Creating a Request
    GetCSVHeader (..),
    newGetCSVHeader,

    -- * Request Lenses
    getCSVHeader_userPoolId,

    -- * Destructuring the Response
    GetCSVHeaderResponse (..),
    newGetCSVHeaderResponse,

    -- * Response Lenses
    getCSVHeaderResponse_cSVHeader,
    getCSVHeaderResponse_userPoolId,
    getCSVHeaderResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to get the header information of the CSV file for
-- the user import job.
--
-- /See:/ 'newGetCSVHeader' smart constructor.
data GetCSVHeader = GetCSVHeader'
  { -- | The user pool ID for the user pool that the users are to be imported
    -- into.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCSVHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'getCSVHeader_userPoolId' - The user pool ID for the user pool that the users are to be imported
-- into.
newGetCSVHeader ::
  -- | 'userPoolId'
  Prelude.Text ->
  GetCSVHeader
newGetCSVHeader pUserPoolId_ =
  GetCSVHeader' {userPoolId = pUserPoolId_}

-- | The user pool ID for the user pool that the users are to be imported
-- into.
getCSVHeader_userPoolId :: Lens.Lens' GetCSVHeader Prelude.Text
getCSVHeader_userPoolId = Lens.lens (\GetCSVHeader' {userPoolId} -> userPoolId) (\s@GetCSVHeader' {} a -> s {userPoolId = a} :: GetCSVHeader)

instance Core.AWSRequest GetCSVHeader where
  type AWSResponse GetCSVHeader = GetCSVHeaderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCSVHeaderResponse'
            Prelude.<$> (x Data..?> "CSVHeader" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "UserPoolId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCSVHeader where
  hashWithSalt _salt GetCSVHeader' {..} =
    _salt `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData GetCSVHeader where
  rnf GetCSVHeader' {..} = Prelude.rnf userPoolId

instance Data.ToHeaders GetCSVHeader where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.GetCSVHeader" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCSVHeader where
  toJSON GetCSVHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("UserPoolId" Data..= userPoolId)]
      )

instance Data.ToPath GetCSVHeader where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCSVHeader where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to the request to get the header
-- information of the CSV file for the user import job.
--
-- /See:/ 'newGetCSVHeaderResponse' smart constructor.
data GetCSVHeaderResponse = GetCSVHeaderResponse'
  { -- | The header information of the CSV file for the user import job.
    cSVHeader :: Prelude.Maybe [Prelude.Text],
    -- | The user pool ID for the user pool that the users are to be imported
    -- into.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCSVHeaderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cSVHeader', 'getCSVHeaderResponse_cSVHeader' - The header information of the CSV file for the user import job.
--
-- 'userPoolId', 'getCSVHeaderResponse_userPoolId' - The user pool ID for the user pool that the users are to be imported
-- into.
--
-- 'httpStatus', 'getCSVHeaderResponse_httpStatus' - The response's http status code.
newGetCSVHeaderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCSVHeaderResponse
newGetCSVHeaderResponse pHttpStatus_ =
  GetCSVHeaderResponse'
    { cSVHeader = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The header information of the CSV file for the user import job.
getCSVHeaderResponse_cSVHeader :: Lens.Lens' GetCSVHeaderResponse (Prelude.Maybe [Prelude.Text])
getCSVHeaderResponse_cSVHeader = Lens.lens (\GetCSVHeaderResponse' {cSVHeader} -> cSVHeader) (\s@GetCSVHeaderResponse' {} a -> s {cSVHeader = a} :: GetCSVHeaderResponse) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID for the user pool that the users are to be imported
-- into.
getCSVHeaderResponse_userPoolId :: Lens.Lens' GetCSVHeaderResponse (Prelude.Maybe Prelude.Text)
getCSVHeaderResponse_userPoolId = Lens.lens (\GetCSVHeaderResponse' {userPoolId} -> userPoolId) (\s@GetCSVHeaderResponse' {} a -> s {userPoolId = a} :: GetCSVHeaderResponse)

-- | The response's http status code.
getCSVHeaderResponse_httpStatus :: Lens.Lens' GetCSVHeaderResponse Prelude.Int
getCSVHeaderResponse_httpStatus = Lens.lens (\GetCSVHeaderResponse' {httpStatus} -> httpStatus) (\s@GetCSVHeaderResponse' {} a -> s {httpStatus = a} :: GetCSVHeaderResponse)

instance Prelude.NFData GetCSVHeaderResponse where
  rnf GetCSVHeaderResponse' {..} =
    Prelude.rnf cSVHeader `Prelude.seq`
      Prelude.rnf userPoolId `Prelude.seq`
        Prelude.rnf httpStatus
