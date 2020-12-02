{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.TLSConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.TLSConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'tlsConfig' smart constructor.
newtype TLSConfig = TLSConfig'
  { _tcInsecureSkipVerification ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TLSConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcInsecureSkipVerification' - Specifies whether or not API Gateway skips verification that the certificate for an integration endpoint is issued by a <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-supported-certificate-authorities-for-http-endpoints.html supported certificate authority> . This isn’t recommended, but it enables you to use certificates that are signed by private certificate authorities, or certificates that are self-signed. If enabled, API Gateway still performs basic certificate validation, which includes checking the certificate's expiration date, hostname, and presence of a root certificate authority. Supported only for @HTTP@ and @HTTP_PROXY@ integrations.
tlsConfig ::
  TLSConfig
tlsConfig = TLSConfig' {_tcInsecureSkipVerification = Nothing}

-- | Specifies whether or not API Gateway skips verification that the certificate for an integration endpoint is issued by a <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-supported-certificate-authorities-for-http-endpoints.html supported certificate authority> . This isn’t recommended, but it enables you to use certificates that are signed by private certificate authorities, or certificates that are self-signed. If enabled, API Gateway still performs basic certificate validation, which includes checking the certificate's expiration date, hostname, and presence of a root certificate authority. Supported only for @HTTP@ and @HTTP_PROXY@ integrations.
tcInsecureSkipVerification :: Lens' TLSConfig (Maybe Bool)
tcInsecureSkipVerification = lens _tcInsecureSkipVerification (\s a -> s {_tcInsecureSkipVerification = a})

instance FromJSON TLSConfig where
  parseJSON =
    withObject
      "TLSConfig"
      (\x -> TLSConfig' <$> (x .:? "insecureSkipVerification"))

instance Hashable TLSConfig

instance NFData TLSConfig

instance ToJSON TLSConfig where
  toJSON TLSConfig' {..} =
    object
      ( catMaybes
          [("insecureSkipVerification" .=) <$> _tcInsecureSkipVerification]
      )
