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
-- Module      : Amazonka.PinpointEmail.PutEmailIdentityFeedbackAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to enable or disable feedback forwarding for an identity. This
-- setting determines what happens when an identity is used to send an
-- email that results in a bounce or complaint event.
--
-- When you enable feedback forwarding, Amazon Pinpoint sends you email
-- notifications when bounce or complaint events occur. Amazon Pinpoint
-- sends this notification to the address that you specified in the
-- Return-Path header of the original email.
--
-- When you disable feedback forwarding, Amazon Pinpoint sends
-- notifications through other mechanisms, such as by notifying an Amazon
-- SNS topic. You\'re required to have a method of tracking bounces and
-- complaints. If you haven\'t set up another mechanism for receiving
-- bounce or complaint notifications, Amazon Pinpoint sends an email
-- notification when these events occur (even if this setting is disabled).
module Amazonka.PinpointEmail.PutEmailIdentityFeedbackAttributes
  ( -- * Creating a Request
    PutEmailIdentityFeedbackAttributes (..),
    newPutEmailIdentityFeedbackAttributes,

    -- * Request Lenses
    putEmailIdentityFeedbackAttributes_emailForwardingEnabled,
    putEmailIdentityFeedbackAttributes_emailIdentity,

    -- * Destructuring the Response
    PutEmailIdentityFeedbackAttributesResponse (..),
    newPutEmailIdentityFeedbackAttributesResponse,

    -- * Response Lenses
    putEmailIdentityFeedbackAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to set the attributes that control how bounce and complaint
-- events are processed.
--
-- /See:/ 'newPutEmailIdentityFeedbackAttributes' smart constructor.
data PutEmailIdentityFeedbackAttributes = PutEmailIdentityFeedbackAttributes'
  { -- | Sets the feedback forwarding configuration for the identity.
    --
    -- If the value is @true@, Amazon Pinpoint sends you email notifications
    -- when bounce or complaint events occur. Amazon Pinpoint sends this
    -- notification to the address that you specified in the Return-Path header
    -- of the original email.
    --
    -- When you set this value to @false@, Amazon Pinpoint sends notifications
    -- through other mechanisms, such as by notifying an Amazon SNS topic or
    -- another event destination. You\'re required to have a method of tracking
    -- bounces and complaints. If you haven\'t set up another mechanism for
    -- receiving bounce or complaint notifications, Amazon Pinpoint sends an
    -- email notification when these events occur (even if this setting is
    -- disabled).
    emailForwardingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The email identity that you want to configure bounce and complaint
    -- feedback forwarding for.
    emailIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailIdentityFeedbackAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailForwardingEnabled', 'putEmailIdentityFeedbackAttributes_emailForwardingEnabled' - Sets the feedback forwarding configuration for the identity.
--
-- If the value is @true@, Amazon Pinpoint sends you email notifications
-- when bounce or complaint events occur. Amazon Pinpoint sends this
-- notification to the address that you specified in the Return-Path header
-- of the original email.
--
-- When you set this value to @false@, Amazon Pinpoint sends notifications
-- through other mechanisms, such as by notifying an Amazon SNS topic or
-- another event destination. You\'re required to have a method of tracking
-- bounces and complaints. If you haven\'t set up another mechanism for
-- receiving bounce or complaint notifications, Amazon Pinpoint sends an
-- email notification when these events occur (even if this setting is
-- disabled).
--
-- 'emailIdentity', 'putEmailIdentityFeedbackAttributes_emailIdentity' - The email identity that you want to configure bounce and complaint
-- feedback forwarding for.
newPutEmailIdentityFeedbackAttributes ::
  -- | 'emailIdentity'
  Prelude.Text ->
  PutEmailIdentityFeedbackAttributes
newPutEmailIdentityFeedbackAttributes pEmailIdentity_ =
  PutEmailIdentityFeedbackAttributes'
    { emailForwardingEnabled =
        Prelude.Nothing,
      emailIdentity = pEmailIdentity_
    }

-- | Sets the feedback forwarding configuration for the identity.
--
-- If the value is @true@, Amazon Pinpoint sends you email notifications
-- when bounce or complaint events occur. Amazon Pinpoint sends this
-- notification to the address that you specified in the Return-Path header
-- of the original email.
--
-- When you set this value to @false@, Amazon Pinpoint sends notifications
-- through other mechanisms, such as by notifying an Amazon SNS topic or
-- another event destination. You\'re required to have a method of tracking
-- bounces and complaints. If you haven\'t set up another mechanism for
-- receiving bounce or complaint notifications, Amazon Pinpoint sends an
-- email notification when these events occur (even if this setting is
-- disabled).
putEmailIdentityFeedbackAttributes_emailForwardingEnabled :: Lens.Lens' PutEmailIdentityFeedbackAttributes (Prelude.Maybe Prelude.Bool)
putEmailIdentityFeedbackAttributes_emailForwardingEnabled = Lens.lens (\PutEmailIdentityFeedbackAttributes' {emailForwardingEnabled} -> emailForwardingEnabled) (\s@PutEmailIdentityFeedbackAttributes' {} a -> s {emailForwardingEnabled = a} :: PutEmailIdentityFeedbackAttributes)

-- | The email identity that you want to configure bounce and complaint
-- feedback forwarding for.
putEmailIdentityFeedbackAttributes_emailIdentity :: Lens.Lens' PutEmailIdentityFeedbackAttributes Prelude.Text
putEmailIdentityFeedbackAttributes_emailIdentity = Lens.lens (\PutEmailIdentityFeedbackAttributes' {emailIdentity} -> emailIdentity) (\s@PutEmailIdentityFeedbackAttributes' {} a -> s {emailIdentity = a} :: PutEmailIdentityFeedbackAttributes)

instance
  Core.AWSRequest
    PutEmailIdentityFeedbackAttributes
  where
  type
    AWSResponse PutEmailIdentityFeedbackAttributes =
      PutEmailIdentityFeedbackAttributesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutEmailIdentityFeedbackAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutEmailIdentityFeedbackAttributes
  where
  hashWithSalt
    _salt
    PutEmailIdentityFeedbackAttributes' {..} =
      _salt `Prelude.hashWithSalt` emailForwardingEnabled
        `Prelude.hashWithSalt` emailIdentity

instance
  Prelude.NFData
    PutEmailIdentityFeedbackAttributes
  where
  rnf PutEmailIdentityFeedbackAttributes' {..} =
    Prelude.rnf emailForwardingEnabled
      `Prelude.seq` Prelude.rnf emailIdentity

instance
  Data.ToHeaders
    PutEmailIdentityFeedbackAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    PutEmailIdentityFeedbackAttributes
  where
  toJSON PutEmailIdentityFeedbackAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EmailForwardingEnabled" Data..=)
              Prelude.<$> emailForwardingEnabled
          ]
      )

instance
  Data.ToPath
    PutEmailIdentityFeedbackAttributes
  where
  toPath PutEmailIdentityFeedbackAttributes' {..} =
    Prelude.mconcat
      [ "/v1/email/identities/",
        Data.toBS emailIdentity,
        "/feedback"
      ]

instance
  Data.ToQuery
    PutEmailIdentityFeedbackAttributes
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutEmailIdentityFeedbackAttributesResponse' smart constructor.
data PutEmailIdentityFeedbackAttributesResponse = PutEmailIdentityFeedbackAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailIdentityFeedbackAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putEmailIdentityFeedbackAttributesResponse_httpStatus' - The response's http status code.
newPutEmailIdentityFeedbackAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutEmailIdentityFeedbackAttributesResponse
newPutEmailIdentityFeedbackAttributesResponse
  pHttpStatus_ =
    PutEmailIdentityFeedbackAttributesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putEmailIdentityFeedbackAttributesResponse_httpStatus :: Lens.Lens' PutEmailIdentityFeedbackAttributesResponse Prelude.Int
putEmailIdentityFeedbackAttributesResponse_httpStatus = Lens.lens (\PutEmailIdentityFeedbackAttributesResponse' {httpStatus} -> httpStatus) (\s@PutEmailIdentityFeedbackAttributesResponse' {} a -> s {httpStatus = a} :: PutEmailIdentityFeedbackAttributesResponse)

instance
  Prelude.NFData
    PutEmailIdentityFeedbackAttributesResponse
  where
  rnf PutEmailIdentityFeedbackAttributesResponse' {..} =
    Prelude.rnf httpStatus
