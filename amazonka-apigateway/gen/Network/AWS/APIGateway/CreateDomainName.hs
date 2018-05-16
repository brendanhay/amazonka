{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateDomainName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain name.
--
--
module Network.AWS.APIGateway.CreateDomainName
    (
    -- * Creating a Request
      createDomainName
    , CreateDomainName
    -- * Request Lenses
    , cdnCertificateName
    , cdnRegionalCertificateARN
    , cdnCertificateARN
    , cdnCertificatePrivateKey
    , cdnRegionalCertificateName
    , cdnCertificateBody
    , cdnCertificateChain
    , cdnEndpointConfiguration
    , cdnDomainName

    -- * Destructuring the Response
    , domainName
    , DomainName
    -- * Response Lenses
    , dnRegionalHostedZoneId
    , dnCertificateName
    , dnRegionalCertificateARN
    , dnCertificateARN
    , dnDistributionHostedZoneId
    , dnDomainName
    , dnRegionalCertificateName
    , dnRegionalDomainName
    , dnCertificateUploadDate
    , dnDistributionDomainName
    , dnEndpointConfiguration
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to create a new domain name.
--
--
--
-- /See:/ 'createDomainName' smart constructor.
data CreateDomainName = CreateDomainName'
  { _cdnCertificateName         :: !(Maybe Text)
  , _cdnRegionalCertificateARN  :: !(Maybe Text)
  , _cdnCertificateARN          :: !(Maybe Text)
  , _cdnCertificatePrivateKey   :: !(Maybe Text)
  , _cdnRegionalCertificateName :: !(Maybe Text)
  , _cdnCertificateBody         :: !(Maybe Text)
  , _cdnCertificateChain        :: !(Maybe Text)
  , _cdnEndpointConfiguration   :: !(Maybe EndpointConfiguration)
  , _cdnDomainName              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdnCertificateName' - The user-friendly name of the certificate that will be used by edge-optimized endpoint for this domain name.
--
-- * 'cdnRegionalCertificateARN' - The reference to an AWS-managed certificate that will be used by regional endpoint for this domain name. AWS Certificate Manager is the only supported source.
--
-- * 'cdnCertificateARN' - The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
--
-- * 'cdnCertificatePrivateKey' - [Deprecated] Your edge-optimized endpoint's domain name certificate's private key.
--
-- * 'cdnRegionalCertificateName' - The user-friendly name of the certificate that will be used by regional endpoint for this domain name.
--
-- * 'cdnCertificateBody' - [Deprecated] The body of the server certificate that will be used by edge-optimized endpoint for this domain name provided by your certificate authority.
--
-- * 'cdnCertificateChain' - [Deprecated] The intermediate certificates and optionally the root certificate, one after the other without any blank lines, used by an edge-optimized endpoint for this domain name. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
--
-- * 'cdnEndpointConfiguration' - The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
--
-- * 'cdnDomainName' - [Required] The name of the 'DomainName' resource.
createDomainName
    :: Text -- ^ 'cdnDomainName'
    -> CreateDomainName
createDomainName pDomainName_ =
  CreateDomainName'
    { _cdnCertificateName = Nothing
    , _cdnRegionalCertificateARN = Nothing
    , _cdnCertificateARN = Nothing
    , _cdnCertificatePrivateKey = Nothing
    , _cdnRegionalCertificateName = Nothing
    , _cdnCertificateBody = Nothing
    , _cdnCertificateChain = Nothing
    , _cdnEndpointConfiguration = Nothing
    , _cdnDomainName = pDomainName_
    }


-- | The user-friendly name of the certificate that will be used by edge-optimized endpoint for this domain name.
cdnCertificateName :: Lens' CreateDomainName (Maybe Text)
cdnCertificateName = lens _cdnCertificateName (\ s a -> s{_cdnCertificateName = a})

-- | The reference to an AWS-managed certificate that will be used by regional endpoint for this domain name. AWS Certificate Manager is the only supported source.
cdnRegionalCertificateARN :: Lens' CreateDomainName (Maybe Text)
cdnRegionalCertificateARN = lens _cdnRegionalCertificateARN (\ s a -> s{_cdnRegionalCertificateARN = a})

-- | The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
cdnCertificateARN :: Lens' CreateDomainName (Maybe Text)
cdnCertificateARN = lens _cdnCertificateARN (\ s a -> s{_cdnCertificateARN = a})

-- | [Deprecated] Your edge-optimized endpoint's domain name certificate's private key.
cdnCertificatePrivateKey :: Lens' CreateDomainName (Maybe Text)
cdnCertificatePrivateKey = lens _cdnCertificatePrivateKey (\ s a -> s{_cdnCertificatePrivateKey = a})

-- | The user-friendly name of the certificate that will be used by regional endpoint for this domain name.
cdnRegionalCertificateName :: Lens' CreateDomainName (Maybe Text)
cdnRegionalCertificateName = lens _cdnRegionalCertificateName (\ s a -> s{_cdnRegionalCertificateName = a})

-- | [Deprecated] The body of the server certificate that will be used by edge-optimized endpoint for this domain name provided by your certificate authority.
cdnCertificateBody :: Lens' CreateDomainName (Maybe Text)
cdnCertificateBody = lens _cdnCertificateBody (\ s a -> s{_cdnCertificateBody = a})

-- | [Deprecated] The intermediate certificates and optionally the root certificate, one after the other without any blank lines, used by an edge-optimized endpoint for this domain name. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
cdnCertificateChain :: Lens' CreateDomainName (Maybe Text)
cdnCertificateChain = lens _cdnCertificateChain (\ s a -> s{_cdnCertificateChain = a})

-- | The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
cdnEndpointConfiguration :: Lens' CreateDomainName (Maybe EndpointConfiguration)
cdnEndpointConfiguration = lens _cdnEndpointConfiguration (\ s a -> s{_cdnEndpointConfiguration = a})

-- | [Required] The name of the 'DomainName' resource.
cdnDomainName :: Lens' CreateDomainName Text
cdnDomainName = lens _cdnDomainName (\ s a -> s{_cdnDomainName = a})

instance AWSRequest CreateDomainName where
        type Rs CreateDomainName = DomainName
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateDomainName where

instance NFData CreateDomainName where

instance ToHeaders CreateDomainName where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateDomainName where
        toJSON CreateDomainName'{..}
          = object
              (catMaybes
                 [("certificateName" .=) <$> _cdnCertificateName,
                  ("regionalCertificateArn" .=) <$>
                    _cdnRegionalCertificateARN,
                  ("certificateArn" .=) <$> _cdnCertificateARN,
                  ("certificatePrivateKey" .=) <$>
                    _cdnCertificatePrivateKey,
                  ("regionalCertificateName" .=) <$>
                    _cdnRegionalCertificateName,
                  ("certificateBody" .=) <$> _cdnCertificateBody,
                  ("certificateChain" .=) <$> _cdnCertificateChain,
                  ("endpointConfiguration" .=) <$>
                    _cdnEndpointConfiguration,
                  Just ("domainName" .= _cdnDomainName)])

instance ToPath CreateDomainName where
        toPath = const "/domainnames"

instance ToQuery CreateDomainName where
        toQuery = const mempty
