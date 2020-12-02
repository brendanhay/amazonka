{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about a domain configuration.
module Network.AWS.IoT.DescribeDomainConfiguration
  ( -- * Creating a Request
    describeDomainConfiguration,
    DescribeDomainConfiguration,

    -- * Request Lenses
    ddcDomainConfigurationName,

    -- * Destructuring the Response
    describeDomainConfigurationResponse,
    DescribeDomainConfigurationResponse,

    -- * Response Lenses
    ddcrsDomainConfigurationName,
    ddcrsServerCertificates,
    ddcrsAuthorizerConfig,
    ddcrsLastStatusChangeDate,
    ddcrsDomainConfigurationStatus,
    ddcrsDomainName,
    ddcrsDomainConfigurationARN,
    ddcrsServiceType,
    ddcrsDomainType,
    ddcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDomainConfiguration' smart constructor.
newtype DescribeDomainConfiguration = DescribeDomainConfiguration'
  { _ddcDomainConfigurationName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDomainConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcDomainConfigurationName' - The name of the domain configuration.
describeDomainConfiguration ::
  -- | 'ddcDomainConfigurationName'
  Text ->
  DescribeDomainConfiguration
describeDomainConfiguration pDomainConfigurationName_ =
  DescribeDomainConfiguration'
    { _ddcDomainConfigurationName =
        pDomainConfigurationName_
    }

-- | The name of the domain configuration.
ddcDomainConfigurationName :: Lens' DescribeDomainConfiguration Text
ddcDomainConfigurationName = lens _ddcDomainConfigurationName (\s a -> s {_ddcDomainConfigurationName = a})

instance AWSRequest DescribeDomainConfiguration where
  type
    Rs DescribeDomainConfiguration =
      DescribeDomainConfigurationResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeDomainConfigurationResponse'
            <$> (x .?> "domainConfigurationName")
            <*> (x .?> "serverCertificates" .!@ mempty)
            <*> (x .?> "authorizerConfig")
            <*> (x .?> "lastStatusChangeDate")
            <*> (x .?> "domainConfigurationStatus")
            <*> (x .?> "domainName")
            <*> (x .?> "domainConfigurationArn")
            <*> (x .?> "serviceType")
            <*> (x .?> "domainType")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDomainConfiguration

instance NFData DescribeDomainConfiguration

instance ToHeaders DescribeDomainConfiguration where
  toHeaders = const mempty

instance ToPath DescribeDomainConfiguration where
  toPath DescribeDomainConfiguration' {..} =
    mconcat
      ["/domainConfigurations/", toBS _ddcDomainConfigurationName]

instance ToQuery DescribeDomainConfiguration where
  toQuery = const mempty

-- | /See:/ 'describeDomainConfigurationResponse' smart constructor.
data DescribeDomainConfigurationResponse = DescribeDomainConfigurationResponse'
  { _ddcrsDomainConfigurationName ::
      !(Maybe Text),
    _ddcrsServerCertificates ::
      !( Maybe
           [ServerCertificateSummary]
       ),
    _ddcrsAuthorizerConfig ::
      !( Maybe
           AuthorizerConfig
       ),
    _ddcrsLastStatusChangeDate ::
      !(Maybe POSIX),
    _ddcrsDomainConfigurationStatus ::
      !( Maybe
           DomainConfigurationStatus
       ),
    _ddcrsDomainName ::
      !(Maybe Text),
    _ddcrsDomainConfigurationARN ::
      !(Maybe Text),
    _ddcrsServiceType ::
      !( Maybe
           ServiceType
       ),
    _ddcrsDomainType ::
      !(Maybe DomainType),
    _ddcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDomainConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcrsDomainConfigurationName' - The name of the domain configuration.
--
-- * 'ddcrsServerCertificates' - A list containing summary information about the server certificate included in the domain configuration.
--
-- * 'ddcrsAuthorizerConfig' - An object that specifies the authorization service for a domain.
--
-- * 'ddcrsLastStatusChangeDate' - The date and time the domain configuration's status was last changed.
--
-- * 'ddcrsDomainConfigurationStatus' - A Boolean value that specifies the current state of the domain configuration.
--
-- * 'ddcrsDomainName' - The name of the domain.
--
-- * 'ddcrsDomainConfigurationARN' - The ARN of the domain configuration.
--
-- * 'ddcrsServiceType' - The type of service delivered by the endpoint.
--
-- * 'ddcrsDomainType' - The type of the domain.
--
-- * 'ddcrsResponseStatus' - -- | The response status code.
describeDomainConfigurationResponse ::
  -- | 'ddcrsResponseStatus'
  Int ->
  DescribeDomainConfigurationResponse
describeDomainConfigurationResponse pResponseStatus_ =
  DescribeDomainConfigurationResponse'
    { _ddcrsDomainConfigurationName =
        Nothing,
      _ddcrsServerCertificates = Nothing,
      _ddcrsAuthorizerConfig = Nothing,
      _ddcrsLastStatusChangeDate = Nothing,
      _ddcrsDomainConfigurationStatus = Nothing,
      _ddcrsDomainName = Nothing,
      _ddcrsDomainConfigurationARN = Nothing,
      _ddcrsServiceType = Nothing,
      _ddcrsDomainType = Nothing,
      _ddcrsResponseStatus = pResponseStatus_
    }

-- | The name of the domain configuration.
ddcrsDomainConfigurationName :: Lens' DescribeDomainConfigurationResponse (Maybe Text)
ddcrsDomainConfigurationName = lens _ddcrsDomainConfigurationName (\s a -> s {_ddcrsDomainConfigurationName = a})

-- | A list containing summary information about the server certificate included in the domain configuration.
ddcrsServerCertificates :: Lens' DescribeDomainConfigurationResponse [ServerCertificateSummary]
ddcrsServerCertificates = lens _ddcrsServerCertificates (\s a -> s {_ddcrsServerCertificates = a}) . _Default . _Coerce

-- | An object that specifies the authorization service for a domain.
ddcrsAuthorizerConfig :: Lens' DescribeDomainConfigurationResponse (Maybe AuthorizerConfig)
ddcrsAuthorizerConfig = lens _ddcrsAuthorizerConfig (\s a -> s {_ddcrsAuthorizerConfig = a})

-- | The date and time the domain configuration's status was last changed.
ddcrsLastStatusChangeDate :: Lens' DescribeDomainConfigurationResponse (Maybe UTCTime)
ddcrsLastStatusChangeDate = lens _ddcrsLastStatusChangeDate (\s a -> s {_ddcrsLastStatusChangeDate = a}) . mapping _Time

-- | A Boolean value that specifies the current state of the domain configuration.
ddcrsDomainConfigurationStatus :: Lens' DescribeDomainConfigurationResponse (Maybe DomainConfigurationStatus)
ddcrsDomainConfigurationStatus = lens _ddcrsDomainConfigurationStatus (\s a -> s {_ddcrsDomainConfigurationStatus = a})

-- | The name of the domain.
ddcrsDomainName :: Lens' DescribeDomainConfigurationResponse (Maybe Text)
ddcrsDomainName = lens _ddcrsDomainName (\s a -> s {_ddcrsDomainName = a})

-- | The ARN of the domain configuration.
ddcrsDomainConfigurationARN :: Lens' DescribeDomainConfigurationResponse (Maybe Text)
ddcrsDomainConfigurationARN = lens _ddcrsDomainConfigurationARN (\s a -> s {_ddcrsDomainConfigurationARN = a})

-- | The type of service delivered by the endpoint.
ddcrsServiceType :: Lens' DescribeDomainConfigurationResponse (Maybe ServiceType)
ddcrsServiceType = lens _ddcrsServiceType (\s a -> s {_ddcrsServiceType = a})

-- | The type of the domain.
ddcrsDomainType :: Lens' DescribeDomainConfigurationResponse (Maybe DomainType)
ddcrsDomainType = lens _ddcrsDomainType (\s a -> s {_ddcrsDomainType = a})

-- | -- | The response status code.
ddcrsResponseStatus :: Lens' DescribeDomainConfigurationResponse Int
ddcrsResponseStatus = lens _ddcrsResponseStatus (\s a -> s {_ddcrsResponseStatus = a})

instance NFData DescribeDomainConfigurationResponse
