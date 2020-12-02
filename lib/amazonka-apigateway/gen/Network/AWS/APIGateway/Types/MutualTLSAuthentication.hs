{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.MutualTLSAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MutualTLSAuthentication where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | If specified, API Gateway performs two-way authentication between the client and the server. Clients must present a trusted certificate to access your custom domain name.
--
--
--
-- /See:/ 'mutualTLSAuthentication' smart constructor.
data MutualTLSAuthentication = MutualTLSAuthentication'
  { _mtaTruststoreWarnings ::
      !(Maybe [Text]),
    _mtaTruststoreURI :: !(Maybe Text),
    _mtaTruststoreVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MutualTLSAuthentication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtaTruststoreWarnings' - A list of warnings that API Gateway returns while processing your truststore. Invalid certificates produce warnings. Mutual TLS is still enabled, but some clients might not be able to access your API. To resolve warnings, upload a new truststore to S3, and then update you domain name to use the new version.
--
-- * 'mtaTruststoreURI' - An Amazon S3 URL that specifies the truststore for mutual TLS authentication, for example @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
--
-- * 'mtaTruststoreVersion' - The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
mutualTLSAuthentication ::
  MutualTLSAuthentication
mutualTLSAuthentication =
  MutualTLSAuthentication'
    { _mtaTruststoreWarnings = Nothing,
      _mtaTruststoreURI = Nothing,
      _mtaTruststoreVersion = Nothing
    }

-- | A list of warnings that API Gateway returns while processing your truststore. Invalid certificates produce warnings. Mutual TLS is still enabled, but some clients might not be able to access your API. To resolve warnings, upload a new truststore to S3, and then update you domain name to use the new version.
mtaTruststoreWarnings :: Lens' MutualTLSAuthentication [Text]
mtaTruststoreWarnings = lens _mtaTruststoreWarnings (\s a -> s {_mtaTruststoreWarnings = a}) . _Default . _Coerce

-- | An Amazon S3 URL that specifies the truststore for mutual TLS authentication, for example @s3://bucket-name/key-name@ . The truststore can contain certificates from public or private certificate authorities. To update the truststore, upload a new version to S3, and then update your custom domain name to use the new version. To update the truststore, you must have permissions to access the S3 object.
mtaTruststoreURI :: Lens' MutualTLSAuthentication (Maybe Text)
mtaTruststoreURI = lens _mtaTruststoreURI (\s a -> s {_mtaTruststoreURI = a})

-- | The version of the S3 object that contains your truststore. To specify a version, you must have versioning enabled for the S3 bucket.
mtaTruststoreVersion :: Lens' MutualTLSAuthentication (Maybe Text)
mtaTruststoreVersion = lens _mtaTruststoreVersion (\s a -> s {_mtaTruststoreVersion = a})

instance FromJSON MutualTLSAuthentication where
  parseJSON =
    withObject
      "MutualTLSAuthentication"
      ( \x ->
          MutualTLSAuthentication'
            <$> (x .:? "truststoreWarnings" .!= mempty)
            <*> (x .:? "truststoreUri")
            <*> (x .:? "truststoreVersion")
      )

instance Hashable MutualTLSAuthentication

instance NFData MutualTLSAuthentication
