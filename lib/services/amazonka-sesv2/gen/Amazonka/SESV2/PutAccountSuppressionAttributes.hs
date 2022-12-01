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
-- Module      : Amazonka.SESV2.PutAccountSuppressionAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Change the settings for the account-level suppression list.
module Amazonka.SESV2.PutAccountSuppressionAttributes
  ( -- * Creating a Request
    PutAccountSuppressionAttributes (..),
    newPutAccountSuppressionAttributes,

    -- * Request Lenses
    putAccountSuppressionAttributes_suppressedReasons,

    -- * Destructuring the Response
    PutAccountSuppressionAttributesResponse (..),
    newPutAccountSuppressionAttributesResponse,

    -- * Response Lenses
    putAccountSuppressionAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to change your account\'s suppression preferences.
--
-- /See:/ 'newPutAccountSuppressionAttributes' smart constructor.
data PutAccountSuppressionAttributes = PutAccountSuppressionAttributes'
  { -- | A list that contains the reasons that email addresses will be
    -- automatically added to the suppression list for your account. This list
    -- can contain any or all of the following:
    --
    -- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
    --     list for your account when a message sent to that address results in
    --     a complaint.
    --
    -- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
    --     for your account when a message sent to that address results in a
    --     hard bounce.
    suppressedReasons :: Prelude.Maybe [SuppressionListReason]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountSuppressionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suppressedReasons', 'putAccountSuppressionAttributes_suppressedReasons' - A list that contains the reasons that email addresses will be
-- automatically added to the suppression list for your account. This list
-- can contain any or all of the following:
--
-- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
--     list for your account when a message sent to that address results in
--     a complaint.
--
-- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
--     for your account when a message sent to that address results in a
--     hard bounce.
newPutAccountSuppressionAttributes ::
  PutAccountSuppressionAttributes
newPutAccountSuppressionAttributes =
  PutAccountSuppressionAttributes'
    { suppressedReasons =
        Prelude.Nothing
    }

-- | A list that contains the reasons that email addresses will be
-- automatically added to the suppression list for your account. This list
-- can contain any or all of the following:
--
-- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
--     list for your account when a message sent to that address results in
--     a complaint.
--
-- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
--     for your account when a message sent to that address results in a
--     hard bounce.
putAccountSuppressionAttributes_suppressedReasons :: Lens.Lens' PutAccountSuppressionAttributes (Prelude.Maybe [SuppressionListReason])
putAccountSuppressionAttributes_suppressedReasons = Lens.lens (\PutAccountSuppressionAttributes' {suppressedReasons} -> suppressedReasons) (\s@PutAccountSuppressionAttributes' {} a -> s {suppressedReasons = a} :: PutAccountSuppressionAttributes) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    PutAccountSuppressionAttributes
  where
  type
    AWSResponse PutAccountSuppressionAttributes =
      PutAccountSuppressionAttributesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccountSuppressionAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutAccountSuppressionAttributes
  where
  hashWithSalt
    _salt
    PutAccountSuppressionAttributes' {..} =
      _salt `Prelude.hashWithSalt` suppressedReasons

instance
  Prelude.NFData
    PutAccountSuppressionAttributes
  where
  rnf PutAccountSuppressionAttributes' {..} =
    Prelude.rnf suppressedReasons

instance
  Core.ToHeaders
    PutAccountSuppressionAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutAccountSuppressionAttributes where
  toJSON PutAccountSuppressionAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SuppressedReasons" Core..=)
              Prelude.<$> suppressedReasons
          ]
      )

instance Core.ToPath PutAccountSuppressionAttributes where
  toPath =
    Prelude.const "/v2/email/account/suppression"

instance Core.ToQuery PutAccountSuppressionAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutAccountSuppressionAttributesResponse' smart constructor.
data PutAccountSuppressionAttributesResponse = PutAccountSuppressionAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountSuppressionAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAccountSuppressionAttributesResponse_httpStatus' - The response's http status code.
newPutAccountSuppressionAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccountSuppressionAttributesResponse
newPutAccountSuppressionAttributesResponse
  pHttpStatus_ =
    PutAccountSuppressionAttributesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putAccountSuppressionAttributesResponse_httpStatus :: Lens.Lens' PutAccountSuppressionAttributesResponse Prelude.Int
putAccountSuppressionAttributesResponse_httpStatus = Lens.lens (\PutAccountSuppressionAttributesResponse' {httpStatus} -> httpStatus) (\s@PutAccountSuppressionAttributesResponse' {} a -> s {httpStatus = a} :: PutAccountSuppressionAttributesResponse)

instance
  Prelude.NFData
    PutAccountSuppressionAttributesResponse
  where
  rnf PutAccountSuppressionAttributesResponse' {..} =
    Prelude.rnf httpStatus
