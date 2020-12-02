{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.MessageDsn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.MessageDsn where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.ExtensionField

-- | Message-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'messageDsn' smart constructor.
data MessageDsn = MessageDsn'
  { _mdArrivalDate :: !(Maybe ISO8601),
    _mdExtensionFields :: !(Maybe [ExtensionField]),
    _mdReportingMta :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageDsn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdArrivalDate' - When the message was received by the reporting mail transfer agent (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
--
-- * 'mdExtensionFields' - Additional X-headers to include in the DSN.
--
-- * 'mdReportingMta' - The reporting MTA that attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). The default value is @dns; inbound-smtp.[region].amazonaws.com@ .
messageDsn ::
  -- | 'mdReportingMta'
  Text ->
  MessageDsn
messageDsn pReportingMta_ =
  MessageDsn'
    { _mdArrivalDate = Nothing,
      _mdExtensionFields = Nothing,
      _mdReportingMta = pReportingMta_
    }

-- | When the message was received by the reporting mail transfer agent (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
mdArrivalDate :: Lens' MessageDsn (Maybe UTCTime)
mdArrivalDate = lens _mdArrivalDate (\s a -> s {_mdArrivalDate = a}) . mapping _Time

-- | Additional X-headers to include in the DSN.
mdExtensionFields :: Lens' MessageDsn [ExtensionField]
mdExtensionFields = lens _mdExtensionFields (\s a -> s {_mdExtensionFields = a}) . _Default . _Coerce

-- | The reporting MTA that attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). The default value is @dns; inbound-smtp.[region].amazonaws.com@ .
mdReportingMta :: Lens' MessageDsn Text
mdReportingMta = lens _mdReportingMta (\s a -> s {_mdReportingMta = a})

instance Hashable MessageDsn

instance NFData MessageDsn

instance ToQuery MessageDsn where
  toQuery MessageDsn' {..} =
    mconcat
      [ "ArrivalDate" =: _mdArrivalDate,
        "ExtensionFields"
          =: toQuery (toQueryList "member" <$> _mdExtensionFields),
        "ReportingMta" =: _mdReportingMta
      ]
