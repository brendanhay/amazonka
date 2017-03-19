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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , cdnCertificateARN
    , cdnCertificatePrivateKey
    , cdnCertificateBody
    , cdnCertificateChain
    , cdnDomainName

    -- * Destructuring the Response
    , domainName
    , DomainName
    -- * Response Lenses
    , dnCertificateName
    , dnCertificateARN
    , dnDomainName
    , dnCertificateUploadDate
    , dnDistributionDomainName
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to create a new domain name.
--
--
--
-- /See:/ 'createDomainName' smart constructor.
data CreateDomainName = CreateDomainName'
    { _cdnCertificateName       :: !(Maybe Text)
    , _cdnCertificateARN        :: !(Maybe Text)
    , _cdnCertificatePrivateKey :: !(Maybe Text)
    , _cdnCertificateBody       :: !(Maybe Text)
    , _cdnCertificateChain      :: !(Maybe Text)
    , _cdnDomainName            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdnCertificateName' - The user-friendly name of the certificate.
--
-- * 'cdnCertificateARN' - The reference to an AWS-managed certificate. AWS Certificate Manager is the only supported source.
--
-- * 'cdnCertificatePrivateKey' - [Deprecated] Your certificate's private key.
--
-- * 'cdnCertificateBody' - [Deprecated] The body of the server certificate provided by your certificate authority.
--
-- * 'cdnCertificateChain' - [Deprecated] The intermediate certificates and optionally the root certificate, one after the other without any blank lines. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
--
-- * 'cdnDomainName' - (Required) The name of the 'DomainName' resource.
createDomainName
    :: Text -- ^ 'cdnDomainName'
    -> CreateDomainName
createDomainName pDomainName_ =
    CreateDomainName'
    { _cdnCertificateName = Nothing
    , _cdnCertificateARN = Nothing
    , _cdnCertificatePrivateKey = Nothing
    , _cdnCertificateBody = Nothing
    , _cdnCertificateChain = Nothing
    , _cdnDomainName = pDomainName_
    }

-- | The user-friendly name of the certificate.
cdnCertificateName :: Lens' CreateDomainName (Maybe Text)
cdnCertificateName = lens _cdnCertificateName (\ s a -> s{_cdnCertificateName = a});

-- | The reference to an AWS-managed certificate. AWS Certificate Manager is the only supported source.
cdnCertificateARN :: Lens' CreateDomainName (Maybe Text)
cdnCertificateARN = lens _cdnCertificateARN (\ s a -> s{_cdnCertificateARN = a});

-- | [Deprecated] Your certificate's private key.
cdnCertificatePrivateKey :: Lens' CreateDomainName (Maybe Text)
cdnCertificatePrivateKey = lens _cdnCertificatePrivateKey (\ s a -> s{_cdnCertificatePrivateKey = a});

-- | [Deprecated] The body of the server certificate provided by your certificate authority.
cdnCertificateBody :: Lens' CreateDomainName (Maybe Text)
cdnCertificateBody = lens _cdnCertificateBody (\ s a -> s{_cdnCertificateBody = a});

-- | [Deprecated] The intermediate certificates and optionally the root certificate, one after the other without any blank lines. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
cdnCertificateChain :: Lens' CreateDomainName (Maybe Text)
cdnCertificateChain = lens _cdnCertificateChain (\ s a -> s{_cdnCertificateChain = a});

-- | (Required) The name of the 'DomainName' resource.
cdnDomainName :: Lens' CreateDomainName Text
cdnDomainName = lens _cdnDomainName (\ s a -> s{_cdnDomainName = a});

instance AWSRequest CreateDomainName where
        type Rs CreateDomainName = DomainName
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateDomainName

instance NFData CreateDomainName

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
                  ("certificateArn" .=) <$> _cdnCertificateARN,
                  ("certificatePrivateKey" .=) <$>
                    _cdnCertificatePrivateKey,
                  ("certificateBody" .=) <$> _cdnCertificateBody,
                  ("certificateChain" .=) <$> _cdnCertificateChain,
                  Just ("domainName" .= _cdnDomainName)])

instance ToPath CreateDomainName where
        toPath = const "/domainnames"

instance ToQuery CreateDomainName where
        toQuery = const mempty
