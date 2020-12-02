{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Workforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Workforce where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CognitoConfig
import Network.AWS.SageMaker.Types.OidcConfigForResponse
import Network.AWS.SageMaker.Types.SourceIPConfig

-- | A single private workforce, which is automatically created when you create your first private work team. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
--
--
--
-- /See:/ 'workforce' smart constructor.
data Workforce = Workforce'
  { _wSubDomain :: !(Maybe Text),
    _wCreateDate :: !(Maybe POSIX),
    _wSourceIPConfig :: !(Maybe SourceIPConfig),
    _wCognitoConfig :: !(Maybe CognitoConfig),
    _wLastUpdatedDate :: !(Maybe POSIX),
    _wOidcConfig :: !(Maybe OidcConfigForResponse),
    _wWorkforceName :: !Text,
    _wWorkforceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Workforce' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wSubDomain' - The subdomain for your OIDC Identity Provider.
--
-- * 'wCreateDate' - The date that the workforce is created.
--
-- * 'wSourceIPConfig' - A list of one to ten IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to be added to the workforce allow list. By default, a workforce isn't restricted to specific IP addresses.
--
-- * 'wCognitoConfig' - The configuration of an Amazon Cognito workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
--
-- * 'wLastUpdatedDate' - The most recent date that was used to successfully add one or more IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to a private workforce's allow list.
--
-- * 'wOidcConfig' - The configuration of an OIDC Identity Provider (IdP) private workforce.
--
-- * 'wWorkforceName' - The name of the private workforce.
--
-- * 'wWorkforceARN' - The Amazon Resource Name (ARN) of the private workforce.
workforce ::
  -- | 'wWorkforceName'
  Text ->
  -- | 'wWorkforceARN'
  Text ->
  Workforce
workforce pWorkforceName_ pWorkforceARN_ =
  Workforce'
    { _wSubDomain = Nothing,
      _wCreateDate = Nothing,
      _wSourceIPConfig = Nothing,
      _wCognitoConfig = Nothing,
      _wLastUpdatedDate = Nothing,
      _wOidcConfig = Nothing,
      _wWorkforceName = pWorkforceName_,
      _wWorkforceARN = pWorkforceARN_
    }

-- | The subdomain for your OIDC Identity Provider.
wSubDomain :: Lens' Workforce (Maybe Text)
wSubDomain = lens _wSubDomain (\s a -> s {_wSubDomain = a})

-- | The date that the workforce is created.
wCreateDate :: Lens' Workforce (Maybe UTCTime)
wCreateDate = lens _wCreateDate (\s a -> s {_wCreateDate = a}) . mapping _Time

-- | A list of one to ten IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to be added to the workforce allow list. By default, a workforce isn't restricted to specific IP addresses.
wSourceIPConfig :: Lens' Workforce (Maybe SourceIPConfig)
wSourceIPConfig = lens _wSourceIPConfig (\s a -> s {_wSourceIPConfig = a})

-- | The configuration of an Amazon Cognito workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
wCognitoConfig :: Lens' Workforce (Maybe CognitoConfig)
wCognitoConfig = lens _wCognitoConfig (\s a -> s {_wCognitoConfig = a})

-- | The most recent date that was used to successfully add one or more IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to a private workforce's allow list.
wLastUpdatedDate :: Lens' Workforce (Maybe UTCTime)
wLastUpdatedDate = lens _wLastUpdatedDate (\s a -> s {_wLastUpdatedDate = a}) . mapping _Time

-- | The configuration of an OIDC Identity Provider (IdP) private workforce.
wOidcConfig :: Lens' Workforce (Maybe OidcConfigForResponse)
wOidcConfig = lens _wOidcConfig (\s a -> s {_wOidcConfig = a})

-- | The name of the private workforce.
wWorkforceName :: Lens' Workforce Text
wWorkforceName = lens _wWorkforceName (\s a -> s {_wWorkforceName = a})

-- | The Amazon Resource Name (ARN) of the private workforce.
wWorkforceARN :: Lens' Workforce Text
wWorkforceARN = lens _wWorkforceARN (\s a -> s {_wWorkforceARN = a})

instance FromJSON Workforce where
  parseJSON =
    withObject
      "Workforce"
      ( \x ->
          Workforce'
            <$> (x .:? "SubDomain")
            <*> (x .:? "CreateDate")
            <*> (x .:? "SourceIpConfig")
            <*> (x .:? "CognitoConfig")
            <*> (x .:? "LastUpdatedDate")
            <*> (x .:? "OidcConfig")
            <*> (x .: "WorkforceName")
            <*> (x .: "WorkforceArn")
      )

instance Hashable Workforce

instance NFData Workforce
