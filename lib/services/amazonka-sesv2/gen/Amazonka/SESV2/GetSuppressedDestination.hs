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
-- Module      : Amazonka.SESV2.GetSuppressedDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specific email address that\'s on the
-- suppression list for your account.
module Amazonka.SESV2.GetSuppressedDestination
  ( -- * Creating a Request
    GetSuppressedDestination (..),
    newGetSuppressedDestination,

    -- * Request Lenses
    getSuppressedDestination_emailAddress,

    -- * Destructuring the Response
    GetSuppressedDestinationResponse (..),
    newGetSuppressedDestinationResponse,

    -- * Response Lenses
    getSuppressedDestinationResponse_httpStatus,
    getSuppressedDestinationResponse_suppressedDestination,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to retrieve information about an email address that\'s on the
-- suppression list for your account.
--
-- /See:/ 'newGetSuppressedDestination' smart constructor.
data GetSuppressedDestination = GetSuppressedDestination'
  { -- | The email address that\'s on the account suppression list.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSuppressedDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailAddress', 'getSuppressedDestination_emailAddress' - The email address that\'s on the account suppression list.
newGetSuppressedDestination ::
  -- | 'emailAddress'
  Prelude.Text ->
  GetSuppressedDestination
newGetSuppressedDestination pEmailAddress_ =
  GetSuppressedDestination'
    { emailAddress =
        pEmailAddress_
    }

-- | The email address that\'s on the account suppression list.
getSuppressedDestination_emailAddress :: Lens.Lens' GetSuppressedDestination Prelude.Text
getSuppressedDestination_emailAddress = Lens.lens (\GetSuppressedDestination' {emailAddress} -> emailAddress) (\s@GetSuppressedDestination' {} a -> s {emailAddress = a} :: GetSuppressedDestination)

instance Core.AWSRequest GetSuppressedDestination where
  type
    AWSResponse GetSuppressedDestination =
      GetSuppressedDestinationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSuppressedDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "SuppressedDestination")
      )

instance Prelude.Hashable GetSuppressedDestination where
  hashWithSalt _salt GetSuppressedDestination' {..} =
    _salt `Prelude.hashWithSalt` emailAddress

instance Prelude.NFData GetSuppressedDestination where
  rnf GetSuppressedDestination' {..} =
    Prelude.rnf emailAddress

instance Core.ToHeaders GetSuppressedDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSuppressedDestination where
  toPath GetSuppressedDestination' {..} =
    Prelude.mconcat
      [ "/v2/email/suppression/addresses/",
        Core.toBS emailAddress
      ]

instance Core.ToQuery GetSuppressedDestination where
  toQuery = Prelude.const Prelude.mempty

-- | Information about the suppressed email address.
--
-- /See:/ 'newGetSuppressedDestinationResponse' smart constructor.
data GetSuppressedDestinationResponse = GetSuppressedDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object containing information about the suppressed email address.
    suppressedDestination :: SuppressedDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSuppressedDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSuppressedDestinationResponse_httpStatus' - The response's http status code.
--
-- 'suppressedDestination', 'getSuppressedDestinationResponse_suppressedDestination' - An object containing information about the suppressed email address.
newGetSuppressedDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'suppressedDestination'
  SuppressedDestination ->
  GetSuppressedDestinationResponse
newGetSuppressedDestinationResponse
  pHttpStatus_
  pSuppressedDestination_ =
    GetSuppressedDestinationResponse'
      { httpStatus =
          pHttpStatus_,
        suppressedDestination =
          pSuppressedDestination_
      }

-- | The response's http status code.
getSuppressedDestinationResponse_httpStatus :: Lens.Lens' GetSuppressedDestinationResponse Prelude.Int
getSuppressedDestinationResponse_httpStatus = Lens.lens (\GetSuppressedDestinationResponse' {httpStatus} -> httpStatus) (\s@GetSuppressedDestinationResponse' {} a -> s {httpStatus = a} :: GetSuppressedDestinationResponse)

-- | An object containing information about the suppressed email address.
getSuppressedDestinationResponse_suppressedDestination :: Lens.Lens' GetSuppressedDestinationResponse SuppressedDestination
getSuppressedDestinationResponse_suppressedDestination = Lens.lens (\GetSuppressedDestinationResponse' {suppressedDestination} -> suppressedDestination) (\s@GetSuppressedDestinationResponse' {} a -> s {suppressedDestination = a} :: GetSuppressedDestinationResponse)

instance
  Prelude.NFData
    GetSuppressedDestinationResponse
  where
  rnf GetSuppressedDestinationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf suppressedDestination
