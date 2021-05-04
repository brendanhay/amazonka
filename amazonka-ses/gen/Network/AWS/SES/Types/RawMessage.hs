{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.RawMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.RawMessage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the raw data of the message.
--
-- /See:/ 'newRawMessage' smart constructor.
data RawMessage = RawMessage'
  { -- | The raw data of the message. This data needs to base64-encoded if you
    -- are accessing Amazon SES directly through the HTTPS interface. If you
    -- are accessing Amazon SES using an AWS SDK, the SDK takes care of the
    -- base 64-encoding for you. In all cases, the client must ensure that the
    -- message format complies with Internet email standards regarding email
    -- header fields, MIME types, and MIME encoding.
    --
    -- The To:, CC:, and BCC: headers in the raw message can contain a group
    -- list.
    --
    -- If you are using @SendRawEmail@ with sending authorization, you can
    -- include X-headers in the raw message to specify the \"Source,\"
    -- \"From,\" and \"Return-Path\" addresses. For more information, see the
    -- documentation for @SendRawEmail@.
    --
    -- Do not include these X-headers in the DKIM signature, because they are
    -- removed by Amazon SES before sending the email.
    --
    -- For more information, go to the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide>.
    data' :: Prelude.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RawMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'rawMessage_data' - The raw data of the message. This data needs to base64-encoded if you
-- are accessing Amazon SES directly through the HTTPS interface. If you
-- are accessing Amazon SES using an AWS SDK, the SDK takes care of the
-- base 64-encoding for you. In all cases, the client must ensure that the
-- message format complies with Internet email standards regarding email
-- header fields, MIME types, and MIME encoding.
--
-- The To:, CC:, and BCC: headers in the raw message can contain a group
-- list.
--
-- If you are using @SendRawEmail@ with sending authorization, you can
-- include X-headers in the raw message to specify the \"Source,\"
-- \"From,\" and \"Return-Path\" addresses. For more information, see the
-- documentation for @SendRawEmail@.
--
-- Do not include these X-headers in the DKIM signature, because they are
-- removed by Amazon SES before sending the email.
--
-- For more information, go to the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide>.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newRawMessage ::
  -- | 'data''
  Prelude.ByteString ->
  RawMessage
newRawMessage pData_ =
  RawMessage' {data' = Prelude._Base64 Lens.# pData_}

-- | The raw data of the message. This data needs to base64-encoded if you
-- are accessing Amazon SES directly through the HTTPS interface. If you
-- are accessing Amazon SES using an AWS SDK, the SDK takes care of the
-- base 64-encoding for you. In all cases, the client must ensure that the
-- message format complies with Internet email standards regarding email
-- header fields, MIME types, and MIME encoding.
--
-- The To:, CC:, and BCC: headers in the raw message can contain a group
-- list.
--
-- If you are using @SendRawEmail@ with sending authorization, you can
-- include X-headers in the raw message to specify the \"Source,\"
-- \"From,\" and \"Return-Path\" addresses. For more information, see the
-- documentation for @SendRawEmail@.
--
-- Do not include these X-headers in the DKIM signature, because they are
-- removed by Amazon SES before sending the email.
--
-- For more information, go to the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide>.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
rawMessage_data :: Lens.Lens' RawMessage Prelude.ByteString
rawMessage_data = Lens.lens (\RawMessage' {data'} -> data') (\s@RawMessage' {} a -> s {data' = a} :: RawMessage) Prelude.. Prelude._Base64

instance Prelude.Hashable RawMessage

instance Prelude.NFData RawMessage

instance Prelude.ToQuery RawMessage where
  toQuery RawMessage' {..} =
    Prelude.mconcat ["Data" Prelude.=: data']
