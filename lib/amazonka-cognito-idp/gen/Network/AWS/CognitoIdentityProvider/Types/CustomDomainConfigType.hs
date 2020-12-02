{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
--
--
--
-- /See:/ 'customDomainConfigType' smart constructor.
newtype CustomDomainConfigType = CustomDomainConfigType'
  { _cdctCertificateARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomDomainConfigType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdctCertificateARN' - The Amazon Resource Name (ARN) of an AWS Certificate Manager SSL certificate. You use this certificate for the subdomain of your custom domain.
customDomainConfigType ::
  -- | 'cdctCertificateARN'
  Text ->
  CustomDomainConfigType
customDomainConfigType pCertificateARN_ =
  CustomDomainConfigType' {_cdctCertificateARN = pCertificateARN_}

-- | The Amazon Resource Name (ARN) of an AWS Certificate Manager SSL certificate. You use this certificate for the subdomain of your custom domain.
cdctCertificateARN :: Lens' CustomDomainConfigType Text
cdctCertificateARN = lens _cdctCertificateARN (\s a -> s {_cdctCertificateARN = a})

instance FromJSON CustomDomainConfigType where
  parseJSON =
    withObject
      "CustomDomainConfigType"
      (\x -> CustomDomainConfigType' <$> (x .: "CertificateArn"))

instance Hashable CustomDomainConfigType

instance NFData CustomDomainConfigType

instance ToJSON CustomDomainConfigType where
  toJSON CustomDomainConfigType' {..} =
    object
      (catMaybes [Just ("CertificateArn" .= _cdctCertificateARN)])
