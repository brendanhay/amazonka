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
-- Module      : Network.AWS.SESv2.PutSuppressedDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an email address to the suppression list for your account.
module Network.AWS.SESv2.PutSuppressedDestination
  ( -- * Creating a Request
    PutSuppressedDestination (..),
    newPutSuppressedDestination,

    -- * Request Lenses
    putSuppressedDestination_emailAddress,
    putSuppressedDestination_reason,

    -- * Destructuring the Response
    PutSuppressedDestinationResponse (..),
    newPutSuppressedDestinationResponse,

    -- * Response Lenses
    putSuppressedDestinationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to add an email destination to the suppression list for your
-- account.
--
-- /See:/ 'newPutSuppressedDestination' smart constructor.
data PutSuppressedDestination = PutSuppressedDestination'
  { -- | The email address that should be added to the suppression list for your
    -- account.
    emailAddress :: Prelude.Text,
    -- | The factors that should cause the email address to be added to the
    -- suppression list for your account.
    reason :: SuppressionListReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSuppressedDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailAddress', 'putSuppressedDestination_emailAddress' - The email address that should be added to the suppression list for your
-- account.
--
-- 'reason', 'putSuppressedDestination_reason' - The factors that should cause the email address to be added to the
-- suppression list for your account.
newPutSuppressedDestination ::
  -- | 'emailAddress'
  Prelude.Text ->
  -- | 'reason'
  SuppressionListReason ->
  PutSuppressedDestination
newPutSuppressedDestination pEmailAddress_ pReason_ =
  PutSuppressedDestination'
    { emailAddress =
        pEmailAddress_,
      reason = pReason_
    }

-- | The email address that should be added to the suppression list for your
-- account.
putSuppressedDestination_emailAddress :: Lens.Lens' PutSuppressedDestination Prelude.Text
putSuppressedDestination_emailAddress = Lens.lens (\PutSuppressedDestination' {emailAddress} -> emailAddress) (\s@PutSuppressedDestination' {} a -> s {emailAddress = a} :: PutSuppressedDestination)

-- | The factors that should cause the email address to be added to the
-- suppression list for your account.
putSuppressedDestination_reason :: Lens.Lens' PutSuppressedDestination SuppressionListReason
putSuppressedDestination_reason = Lens.lens (\PutSuppressedDestination' {reason} -> reason) (\s@PutSuppressedDestination' {} a -> s {reason = a} :: PutSuppressedDestination)

instance Core.AWSRequest PutSuppressedDestination where
  type
    AWSResponse PutSuppressedDestination =
      PutSuppressedDestinationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutSuppressedDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutSuppressedDestination

instance Prelude.NFData PutSuppressedDestination

instance Core.ToHeaders PutSuppressedDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutSuppressedDestination where
  toJSON PutSuppressedDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EmailAddress" Core..= emailAddress),
            Prelude.Just ("Reason" Core..= reason)
          ]
      )

instance Core.ToPath PutSuppressedDestination where
  toPath =
    Prelude.const "/v2/email/suppression/addresses"

instance Core.ToQuery PutSuppressedDestination where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutSuppressedDestinationResponse' smart constructor.
data PutSuppressedDestinationResponse = PutSuppressedDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSuppressedDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putSuppressedDestinationResponse_httpStatus' - The response's http status code.
newPutSuppressedDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSuppressedDestinationResponse
newPutSuppressedDestinationResponse pHttpStatus_ =
  PutSuppressedDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putSuppressedDestinationResponse_httpStatus :: Lens.Lens' PutSuppressedDestinationResponse Prelude.Int
putSuppressedDestinationResponse_httpStatus = Lens.lens (\PutSuppressedDestinationResponse' {httpStatus} -> httpStatus) (\s@PutSuppressedDestinationResponse' {} a -> s {httpStatus = a} :: PutSuppressedDestinationResponse)

instance
  Prelude.NFData
    PutSuppressedDestinationResponse
