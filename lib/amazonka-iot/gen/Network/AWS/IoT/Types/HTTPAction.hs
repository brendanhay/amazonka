{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPAction where

import Network.AWS.IoT.Types.HTTPActionHeader
import Network.AWS.IoT.Types.HTTPAuthorization
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Send data to an HTTPS endpoint.
--
--
--
-- /See:/ 'hTTPAction' smart constructor.
data HTTPAction = HTTPAction'
  { _httpaConfirmationURL ::
      !(Maybe Text),
    _httpaAuth :: !(Maybe HTTPAuthorization),
    _httpaHeaders :: !(Maybe [HTTPActionHeader]),
    _httpaUrl :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpaConfirmationURL' - The URL to which AWS IoT sends a confirmation message. The value of the confirmation URL must be a prefix of the endpoint URL. If you do not specify a confirmation URL AWS IoT uses the endpoint URL as the confirmation URL. If you use substitution templates in the confirmationUrl, you must create and enable topic rule destinations that match each possible value of the substitution template before traffic is allowed to your endpoint URL.
--
-- * 'httpaAuth' - The authentication method to use when sending data to an HTTPS endpoint.
--
-- * 'httpaHeaders' - The HTTP headers to send with the message data.
--
-- * 'httpaUrl' - The endpoint URL. If substitution templates are used in the URL, you must also specify a @confirmationUrl@ . If this is a new destination, a new @TopicRuleDestination@ is created if possible.
hTTPAction ::
  -- | 'httpaUrl'
  Text ->
  HTTPAction
hTTPAction pUrl_ =
  HTTPAction'
    { _httpaConfirmationURL = Nothing,
      _httpaAuth = Nothing,
      _httpaHeaders = Nothing,
      _httpaUrl = pUrl_
    }

-- | The URL to which AWS IoT sends a confirmation message. The value of the confirmation URL must be a prefix of the endpoint URL. If you do not specify a confirmation URL AWS IoT uses the endpoint URL as the confirmation URL. If you use substitution templates in the confirmationUrl, you must create and enable topic rule destinations that match each possible value of the substitution template before traffic is allowed to your endpoint URL.
httpaConfirmationURL :: Lens' HTTPAction (Maybe Text)
httpaConfirmationURL = lens _httpaConfirmationURL (\s a -> s {_httpaConfirmationURL = a})

-- | The authentication method to use when sending data to an HTTPS endpoint.
httpaAuth :: Lens' HTTPAction (Maybe HTTPAuthorization)
httpaAuth = lens _httpaAuth (\s a -> s {_httpaAuth = a})

-- | The HTTP headers to send with the message data.
httpaHeaders :: Lens' HTTPAction [HTTPActionHeader]
httpaHeaders = lens _httpaHeaders (\s a -> s {_httpaHeaders = a}) . _Default . _Coerce

-- | The endpoint URL. If substitution templates are used in the URL, you must also specify a @confirmationUrl@ . If this is a new destination, a new @TopicRuleDestination@ is created if possible.
httpaUrl :: Lens' HTTPAction Text
httpaUrl = lens _httpaUrl (\s a -> s {_httpaUrl = a})

instance FromJSON HTTPAction where
  parseJSON =
    withObject
      "HTTPAction"
      ( \x ->
          HTTPAction'
            <$> (x .:? "confirmationUrl")
            <*> (x .:? "auth")
            <*> (x .:? "headers" .!= mempty)
            <*> (x .: "url")
      )

instance Hashable HTTPAction

instance NFData HTTPAction

instance ToJSON HTTPAction where
  toJSON HTTPAction' {..} =
    object
      ( catMaybes
          [ ("confirmationUrl" .=) <$> _httpaConfirmationURL,
            ("auth" .=) <$> _httpaAuth,
            ("headers" .=) <$> _httpaHeaders,
            Just ("url" .= _httpaUrl)
          ]
      )
