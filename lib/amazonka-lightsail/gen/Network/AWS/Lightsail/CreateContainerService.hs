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
-- Module      : Network.AWS.Lightsail.CreateContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Lightsail container service.
--
--
-- A Lightsail container service is a compute resource to which you can deploy containers. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-containers Container services in Amazon Lightsail> in the /Lightsail Dev Guide/ .
module Network.AWS.Lightsail.CreateContainerService
  ( -- * Creating a Request
    createContainerService,
    CreateContainerService,

    -- * Request Lenses
    ccsPublicDomainNames,
    ccsTags,
    ccsDeployment,
    ccsServiceName,
    ccsPower,
    ccsScale,

    -- * Destructuring the Response
    createContainerServiceResponse,
    CreateContainerServiceResponse,

    -- * Response Lenses
    ccsrsContainerService,
    ccsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createContainerService' smart constructor.
data CreateContainerService = CreateContainerService'
  { _ccsPublicDomainNames ::
      !(Maybe (Map Text ([Text]))),
    _ccsTags :: !(Maybe [Tag]),
    _ccsDeployment ::
      !(Maybe ContainerServiceDeploymentRequest),
    _ccsServiceName :: !Text,
    _ccsPower :: !ContainerServicePowerName,
    _ccsScale :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateContainerService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsPublicDomainNames' - The public domain names to use with the container service, such as @example.com@ and @www.example.com@ . You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service. If you don't specify public domain names, then you can use the default domain of the container service. /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service. You can specify public domain names using a string to array map as shown in the example later on this page.
--
-- * 'ccsTags' - The tag keys and optional values for the container service. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- * 'ccsDeployment' - An object that describes a deployment for the container service. A deployment specifies the containers that will be launched on the container service and their settings, such as the ports to open, the environment variables to apply, and the launch command to run. It also specifies the container that will serve as the public endpoint of the deployment and its settings, such as the HTTP or HTTPS port to use, and the health check configuration.
--
-- * 'ccsServiceName' - The name for the container service. The name that you specify for your container service will make up part of its default domain. The default domain of a container service is typically @https://<ServiceName>.<RandomGUID>.<AWSRegion>.cs.amazonlightsail.com@ . If the name of your container service is @container-service-1@ , and it's located in the US East (Ohio) AWS region (@us-east-2@ ), then the domain for your container service will be like the following example: @https://container-service-1.ur4EXAMPLE2uq.us-east-2.cs.amazonlightsail.com@  The following are the requirements for container service names:     * Must be unique within each AWS Region in your Lightsail account.     * Must contain 1 to 63 characters.     * Must contain only alphanumeric characters and hyphens.     * A hyphen (-) can separate words but cannot be at the start or end of the name.
--
-- * 'ccsPower' - The power specification for the container service. The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service. Use the @GetContainerServicePowers@ action to get a list of power options that you can specify using this parameter, and their base monthly cost.
--
-- * 'ccsScale' - The scale specification for the container service. The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
createContainerService ::
  -- | 'ccsServiceName'
  Text ->
  -- | 'ccsPower'
  ContainerServicePowerName ->
  -- | 'ccsScale'
  Natural ->
  CreateContainerService
createContainerService pServiceName_ pPower_ pScale_ =
  CreateContainerService'
    { _ccsPublicDomainNames = Nothing,
      _ccsTags = Nothing,
      _ccsDeployment = Nothing,
      _ccsServiceName = pServiceName_,
      _ccsPower = pPower_,
      _ccsScale = _Nat # pScale_
    }

-- | The public domain names to use with the container service, such as @example.com@ and @www.example.com@ . You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service. If you don't specify public domain names, then you can use the default domain of the container service. /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service. You can specify public domain names using a string to array map as shown in the example later on this page.
ccsPublicDomainNames :: Lens' CreateContainerService (HashMap Text ([Text]))
ccsPublicDomainNames = lens _ccsPublicDomainNames (\s a -> s {_ccsPublicDomainNames = a}) . _Default . _Map

-- | The tag keys and optional values for the container service. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
ccsTags :: Lens' CreateContainerService [Tag]
ccsTags = lens _ccsTags (\s a -> s {_ccsTags = a}) . _Default . _Coerce

-- | An object that describes a deployment for the container service. A deployment specifies the containers that will be launched on the container service and their settings, such as the ports to open, the environment variables to apply, and the launch command to run. It also specifies the container that will serve as the public endpoint of the deployment and its settings, such as the HTTP or HTTPS port to use, and the health check configuration.
ccsDeployment :: Lens' CreateContainerService (Maybe ContainerServiceDeploymentRequest)
ccsDeployment = lens _ccsDeployment (\s a -> s {_ccsDeployment = a})

-- | The name for the container service. The name that you specify for your container service will make up part of its default domain. The default domain of a container service is typically @https://<ServiceName>.<RandomGUID>.<AWSRegion>.cs.amazonlightsail.com@ . If the name of your container service is @container-service-1@ , and it's located in the US East (Ohio) AWS region (@us-east-2@ ), then the domain for your container service will be like the following example: @https://container-service-1.ur4EXAMPLE2uq.us-east-2.cs.amazonlightsail.com@  The following are the requirements for container service names:     * Must be unique within each AWS Region in your Lightsail account.     * Must contain 1 to 63 characters.     * Must contain only alphanumeric characters and hyphens.     * A hyphen (-) can separate words but cannot be at the start or end of the name.
ccsServiceName :: Lens' CreateContainerService Text
ccsServiceName = lens _ccsServiceName (\s a -> s {_ccsServiceName = a})

-- | The power specification for the container service. The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service. Use the @GetContainerServicePowers@ action to get a list of power options that you can specify using this parameter, and their base monthly cost.
ccsPower :: Lens' CreateContainerService ContainerServicePowerName
ccsPower = lens _ccsPower (\s a -> s {_ccsPower = a})

-- | The scale specification for the container service. The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
ccsScale :: Lens' CreateContainerService Natural
ccsScale = lens _ccsScale (\s a -> s {_ccsScale = a}) . _Nat

instance AWSRequest CreateContainerService where
  type Rs CreateContainerService = CreateContainerServiceResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateContainerServiceResponse'
            <$> (x .?> "containerService") <*> (pure (fromEnum s))
      )

instance Hashable CreateContainerService

instance NFData CreateContainerService

instance ToHeaders CreateContainerService where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.CreateContainerService" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateContainerService where
  toJSON CreateContainerService' {..} =
    object
      ( catMaybes
          [ ("publicDomainNames" .=) <$> _ccsPublicDomainNames,
            ("tags" .=) <$> _ccsTags,
            ("deployment" .=) <$> _ccsDeployment,
            Just ("serviceName" .= _ccsServiceName),
            Just ("power" .= _ccsPower),
            Just ("scale" .= _ccsScale)
          ]
      )

instance ToPath CreateContainerService where
  toPath = const "/"

instance ToQuery CreateContainerService where
  toQuery = const mempty

-- | /See:/ 'createContainerServiceResponse' smart constructor.
data CreateContainerServiceResponse = CreateContainerServiceResponse'
  { _ccsrsContainerService ::
      !(Maybe ContainerService),
    _ccsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateContainerServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsrsContainerService' - An object that describes a container service.
--
-- * 'ccsrsResponseStatus' - -- | The response status code.
createContainerServiceResponse ::
  -- | 'ccsrsResponseStatus'
  Int ->
  CreateContainerServiceResponse
createContainerServiceResponse pResponseStatus_ =
  CreateContainerServiceResponse'
    { _ccsrsContainerService = Nothing,
      _ccsrsResponseStatus = pResponseStatus_
    }

-- | An object that describes a container service.
ccsrsContainerService :: Lens' CreateContainerServiceResponse (Maybe ContainerService)
ccsrsContainerService = lens _ccsrsContainerService (\s a -> s {_ccsrsContainerService = a})

-- | -- | The response status code.
ccsrsResponseStatus :: Lens' CreateContainerServiceResponse Int
ccsrsResponseStatus = lens _ccsrsResponseStatus (\s a -> s {_ccsrsResponseStatus = a})

instance NFData CreateContainerServiceResponse
