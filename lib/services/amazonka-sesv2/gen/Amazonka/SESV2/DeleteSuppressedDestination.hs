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
-- Module      : Amazonka.SESV2.DeleteSuppressedDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an email address from the suppression list for your account.
module Amazonka.SESV2.DeleteSuppressedDestination
  ( -- * Creating a Request
    DeleteSuppressedDestination (..),
    newDeleteSuppressedDestination,

    -- * Request Lenses
    deleteSuppressedDestination_emailAddress,

    -- * Destructuring the Response
    DeleteSuppressedDestinationResponse (..),
    newDeleteSuppressedDestinationResponse,

    -- * Response Lenses
    deleteSuppressedDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to remove an email address from the suppression list for your
-- account.
--
-- /See:/ 'newDeleteSuppressedDestination' smart constructor.
data DeleteSuppressedDestination = DeleteSuppressedDestination'
  { -- | The suppressed email destination to remove from the account suppression
    -- list.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSuppressedDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailAddress', 'deleteSuppressedDestination_emailAddress' - The suppressed email destination to remove from the account suppression
-- list.
newDeleteSuppressedDestination ::
  -- | 'emailAddress'
  Prelude.Text ->
  DeleteSuppressedDestination
newDeleteSuppressedDestination pEmailAddress_ =
  DeleteSuppressedDestination'
    { emailAddress =
        pEmailAddress_
    }

-- | The suppressed email destination to remove from the account suppression
-- list.
deleteSuppressedDestination_emailAddress :: Lens.Lens' DeleteSuppressedDestination Prelude.Text
deleteSuppressedDestination_emailAddress = Lens.lens (\DeleteSuppressedDestination' {emailAddress} -> emailAddress) (\s@DeleteSuppressedDestination' {} a -> s {emailAddress = a} :: DeleteSuppressedDestination)

instance Core.AWSRequest DeleteSuppressedDestination where
  type
    AWSResponse DeleteSuppressedDestination =
      DeleteSuppressedDestinationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSuppressedDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSuppressedDestination where
  hashWithSalt _salt DeleteSuppressedDestination' {..} =
    _salt `Prelude.hashWithSalt` emailAddress

instance Prelude.NFData DeleteSuppressedDestination where
  rnf DeleteSuppressedDestination' {..} =
    Prelude.rnf emailAddress

instance Core.ToHeaders DeleteSuppressedDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteSuppressedDestination where
  toPath DeleteSuppressedDestination' {..} =
    Prelude.mconcat
      [ "/v2/email/suppression/addresses/",
        Core.toBS emailAddress
      ]

instance Core.ToQuery DeleteSuppressedDestination where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newDeleteSuppressedDestinationResponse' smart constructor.
data DeleteSuppressedDestinationResponse = DeleteSuppressedDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSuppressedDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSuppressedDestinationResponse_httpStatus' - The response's http status code.
newDeleteSuppressedDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSuppressedDestinationResponse
newDeleteSuppressedDestinationResponse pHttpStatus_ =
  DeleteSuppressedDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSuppressedDestinationResponse_httpStatus :: Lens.Lens' DeleteSuppressedDestinationResponse Prelude.Int
deleteSuppressedDestinationResponse_httpStatus = Lens.lens (\DeleteSuppressedDestinationResponse' {httpStatus} -> httpStatus) (\s@DeleteSuppressedDestinationResponse' {} a -> s {httpStatus = a} :: DeleteSuppressedDestinationResponse)

instance
  Prelude.NFData
    DeleteSuppressedDestinationResponse
  where
  rnf DeleteSuppressedDestinationResponse' {..} =
    Prelude.rnf httpStatus
