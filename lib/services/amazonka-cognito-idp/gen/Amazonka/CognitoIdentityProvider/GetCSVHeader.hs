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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the header information for the .csv file to be used as input for
-- the user import job.
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
    getCSVHeaderResponse_userPoolId,
    getCSVHeaderResponse_cSVHeader,
    getCSVHeaderResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to get the header information for the .csv file
-- for the user import job.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCSVHeaderResponse'
            Prelude.<$> (x Core..?> "UserPoolId")
            Prelude.<*> (x Core..?> "CSVHeader" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCSVHeader

instance Prelude.NFData GetCSVHeader

instance Core.ToHeaders GetCSVHeader where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetCSVHeader" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCSVHeader where
  toJSON GetCSVHeader' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("UserPoolId" Core..= userPoolId)]
      )

instance Core.ToPath GetCSVHeader where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCSVHeader where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to the request to get the header
-- information for the .csv file for the user import job.
--
-- /See:/ 'newGetCSVHeaderResponse' smart constructor.
data GetCSVHeaderResponse = GetCSVHeaderResponse'
  { -- | The user pool ID for the user pool that the users are to be imported
    -- into.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | The header information for the .csv file for the user import job.
    cSVHeader :: Prelude.Maybe [Prelude.Text],
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
-- 'userPoolId', 'getCSVHeaderResponse_userPoolId' - The user pool ID for the user pool that the users are to be imported
-- into.
--
-- 'cSVHeader', 'getCSVHeaderResponse_cSVHeader' - The header information for the .csv file for the user import job.
--
-- 'httpStatus', 'getCSVHeaderResponse_httpStatus' - The response's http status code.
newGetCSVHeaderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCSVHeaderResponse
newGetCSVHeaderResponse pHttpStatus_ =
  GetCSVHeaderResponse'
    { userPoolId = Prelude.Nothing,
      cSVHeader = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user pool ID for the user pool that the users are to be imported
-- into.
getCSVHeaderResponse_userPoolId :: Lens.Lens' GetCSVHeaderResponse (Prelude.Maybe Prelude.Text)
getCSVHeaderResponse_userPoolId = Lens.lens (\GetCSVHeaderResponse' {userPoolId} -> userPoolId) (\s@GetCSVHeaderResponse' {} a -> s {userPoolId = a} :: GetCSVHeaderResponse)

-- | The header information for the .csv file for the user import job.
getCSVHeaderResponse_cSVHeader :: Lens.Lens' GetCSVHeaderResponse (Prelude.Maybe [Prelude.Text])
getCSVHeaderResponse_cSVHeader = Lens.lens (\GetCSVHeaderResponse' {cSVHeader} -> cSVHeader) (\s@GetCSVHeaderResponse' {} a -> s {cSVHeader = a} :: GetCSVHeaderResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCSVHeaderResponse_httpStatus :: Lens.Lens' GetCSVHeaderResponse Prelude.Int
getCSVHeaderResponse_httpStatus = Lens.lens (\GetCSVHeaderResponse' {httpStatus} -> httpStatus) (\s@GetCSVHeaderResponse' {} a -> s {httpStatus = a} :: GetCSVHeaderResponse)

instance Prelude.NFData GetCSVHeaderResponse
