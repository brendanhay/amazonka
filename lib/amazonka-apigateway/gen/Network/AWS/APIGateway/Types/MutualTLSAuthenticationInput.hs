{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.MutualTLSAuthenticationInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MutualTLSAuthenticationInput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | If specified, API Gateway performs two-way authentication between the client and the server. Clients must present a trusted certificate to access your custom domain name.
--
--
--
-- /See:/ 'mutualTLSAuthenticationInput' smart constructor.
data MutualTLSAuthenticationInput = MutualTLSAuthenticationInput'
  { _mtaiTruststoreURI ::
      !(Maybe Text),
    _mtaiTruststoreVersion ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MutualTLSAuthenticationInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtaiTruststoreURI' - An Amazon S3 resource ARN that specifies the truststore for mutual TLS authentication, for example, @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
--
-- * 'mtaiTruststoreVersion' - The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
mutualTLSAuthenticationInput ::
  MutualTLSAuthenticationInput
mutualTLSAuthenticationInput =
  MutualTLSAuthenticationInput'
    { _mtaiTruststoreURI = Nothing,
      _mtaiTruststoreVersion = Nothing
    }

-- | An Amazon S3 resource ARN that specifies the truststore for mutual TLS authentication, for example, @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
mtaiTruststoreURI :: Lens' MutualTLSAuthenticationInput (Maybe Text)
mtaiTruststoreURI = lens _mtaiTruststoreURI (\s a -> s {_mtaiTruststoreURI = a})

-- | The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
mtaiTruststoreVersion :: Lens' MutualTLSAuthenticationInput (Maybe Text)
mtaiTruststoreVersion = lens _mtaiTruststoreVersion (\s a -> s {_mtaiTruststoreVersion = a})

instance Hashable MutualTLSAuthenticationInput

instance NFData MutualTLSAuthenticationInput

instance ToJSON MutualTLSAuthenticationInput where
  toJSON MutualTLSAuthenticationInput' {..} =
    object
      ( catMaybes
          [ ("truststoreUri" .=) <$> _mtaiTruststoreURI,
            ("truststoreVersion" .=) <$> _mtaiTruststoreVersion
          ]
      )
