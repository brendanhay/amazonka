{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.RawMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.RawMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the raw data of the message.
--
--
--
-- /See:/ 'rawMessage' smart constructor.
newtype RawMessage = RawMessage' {_rmData :: Base64}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RawMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmData' - The raw data of the message. This data needs to base64-encoded if you are accessing Amazon SES directly through the HTTPS interface. If you are accessing Amazon SES using an AWS SDK, the SDK takes care of the base 64-encoding for you. In all cases, the client must ensure that the message format complies with Internet email standards regarding email header fields, MIME types, and MIME encoding. The To:, CC:, and BCC: headers in the raw message can contain a group list. If you are using @SendRawEmail@ with sending authorization, you can include X-headers in the raw message to specify the "Source," "From," and "Return-Path" addresses. For more information, see the documentation for @SendRawEmail@ .  /Important:/ Do not include these X-headers in the DKIM signature, because they are removed by Amazon SES before sending the email. For more information, go to the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide> .-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rawMessage ::
  -- | 'rmData'
  ByteString ->
  RawMessage
rawMessage pData_ = RawMessage' {_rmData = _Base64 # pData_}

-- | The raw data of the message. This data needs to base64-encoded if you are accessing Amazon SES directly through the HTTPS interface. If you are accessing Amazon SES using an AWS SDK, the SDK takes care of the base 64-encoding for you. In all cases, the client must ensure that the message format complies with Internet email standards regarding email header fields, MIME types, and MIME encoding. The To:, CC:, and BCC: headers in the raw message can contain a group list. If you are using @SendRawEmail@ with sending authorization, you can include X-headers in the raw message to specify the "Source," "From," and "Return-Path" addresses. For more information, see the documentation for @SendRawEmail@ .  /Important:/ Do not include these X-headers in the DKIM signature, because they are removed by Amazon SES before sending the email. For more information, go to the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide> .-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rmData :: Lens' RawMessage ByteString
rmData = lens _rmData (\s a -> s {_rmData = a}) . _Base64

instance Hashable RawMessage

instance NFData RawMessage

instance ToQuery RawMessage where
  toQuery RawMessage' {..} = mconcat ["Data" =: _rmData]
