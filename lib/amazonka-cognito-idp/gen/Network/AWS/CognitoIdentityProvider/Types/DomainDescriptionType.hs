{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType where

import Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
import Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for information about a domain.
--
--
--
-- /See:/ 'domainDescriptionType' smart constructor.
data DomainDescriptionType = DomainDescriptionType'
  { _ddtStatus ::
      !(Maybe DomainStatusType),
    _ddtCloudFrontDistribution :: !(Maybe Text),
    _ddtUserPoolId :: !(Maybe Text),
    _ddtDomain :: !(Maybe Text),
    _ddtAWSAccountId :: !(Maybe Text),
    _ddtCustomDomainConfig ::
      !(Maybe CustomDomainConfigType),
    _ddtVersion :: !(Maybe Text),
    _ddtS3Bucket :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainDescriptionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddtStatus' - The domain status.
--
-- * 'ddtCloudFrontDistribution' - The ARN of the CloudFront distribution.
--
-- * 'ddtUserPoolId' - The user pool ID.
--
-- * 'ddtDomain' - The domain string.
--
-- * 'ddtAWSAccountId' - The AWS account ID for the user pool owner.
--
-- * 'ddtCustomDomainConfig' - The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
--
-- * 'ddtVersion' - The app version.
--
-- * 'ddtS3Bucket' - The S3 bucket where the static files for this domain are stored.
domainDescriptionType ::
  DomainDescriptionType
domainDescriptionType =
  DomainDescriptionType'
    { _ddtStatus = Nothing,
      _ddtCloudFrontDistribution = Nothing,
      _ddtUserPoolId = Nothing,
      _ddtDomain = Nothing,
      _ddtAWSAccountId = Nothing,
      _ddtCustomDomainConfig = Nothing,
      _ddtVersion = Nothing,
      _ddtS3Bucket = Nothing
    }

-- | The domain status.
ddtStatus :: Lens' DomainDescriptionType (Maybe DomainStatusType)
ddtStatus = lens _ddtStatus (\s a -> s {_ddtStatus = a})

-- | The ARN of the CloudFront distribution.
ddtCloudFrontDistribution :: Lens' DomainDescriptionType (Maybe Text)
ddtCloudFrontDistribution = lens _ddtCloudFrontDistribution (\s a -> s {_ddtCloudFrontDistribution = a})

-- | The user pool ID.
ddtUserPoolId :: Lens' DomainDescriptionType (Maybe Text)
ddtUserPoolId = lens _ddtUserPoolId (\s a -> s {_ddtUserPoolId = a})

-- | The domain string.
ddtDomain :: Lens' DomainDescriptionType (Maybe Text)
ddtDomain = lens _ddtDomain (\s a -> s {_ddtDomain = a})

-- | The AWS account ID for the user pool owner.
ddtAWSAccountId :: Lens' DomainDescriptionType (Maybe Text)
ddtAWSAccountId = lens _ddtAWSAccountId (\s a -> s {_ddtAWSAccountId = a})

-- | The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
ddtCustomDomainConfig :: Lens' DomainDescriptionType (Maybe CustomDomainConfigType)
ddtCustomDomainConfig = lens _ddtCustomDomainConfig (\s a -> s {_ddtCustomDomainConfig = a})

-- | The app version.
ddtVersion :: Lens' DomainDescriptionType (Maybe Text)
ddtVersion = lens _ddtVersion (\s a -> s {_ddtVersion = a})

-- | The S3 bucket where the static files for this domain are stored.
ddtS3Bucket :: Lens' DomainDescriptionType (Maybe Text)
ddtS3Bucket = lens _ddtS3Bucket (\s a -> s {_ddtS3Bucket = a})

instance FromJSON DomainDescriptionType where
  parseJSON =
    withObject
      "DomainDescriptionType"
      ( \x ->
          DomainDescriptionType'
            <$> (x .:? "Status")
            <*> (x .:? "CloudFrontDistribution")
            <*> (x .:? "UserPoolId")
            <*> (x .:? "Domain")
            <*> (x .:? "AWSAccountId")
            <*> (x .:? "CustomDomainConfig")
            <*> (x .:? "Version")
            <*> (x .:? "S3Bucket")
      )

instance Hashable DomainDescriptionType

instance NFData DomainDescriptionType
