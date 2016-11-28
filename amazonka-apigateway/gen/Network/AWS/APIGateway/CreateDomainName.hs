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
    , cdnDomainName
    , cdnCertificateName
    , cdnCertificateBody
    , cdnCertificatePrivateKey
    , cdnCertificateChain

    -- * Destructuring the Response
    , domainName
    , DomainName
    -- * Response Lenses
    , dnCertificateName
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
    { _cdnDomainName            :: !Text
    , _cdnCertificateName       :: !Text
    , _cdnCertificateBody       :: !Text
    , _cdnCertificatePrivateKey :: !Text
    , _cdnCertificateChain      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdnDomainName' - The name of the 'DomainName' resource.
--
-- * 'cdnCertificateName' - The name of the certificate.
--
-- * 'cdnCertificateBody' - The body of the server certificate provided by your certificate authority.
--
-- * 'cdnCertificatePrivateKey' - Your certificate's private key.
--
-- * 'cdnCertificateChain' - The intermediate certificates and optionally the root certificate, one after the other without any blank lines. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
createDomainName
    :: Text -- ^ 'cdnDomainName'
    -> Text -- ^ 'cdnCertificateName'
    -> Text -- ^ 'cdnCertificateBody'
    -> Text -- ^ 'cdnCertificatePrivateKey'
    -> Text -- ^ 'cdnCertificateChain'
    -> CreateDomainName
createDomainName pDomainName_ pCertificateName_ pCertificateBody_ pCertificatePrivateKey_ pCertificateChain_ =
    CreateDomainName'
    { _cdnDomainName = pDomainName_
    , _cdnCertificateName = pCertificateName_
    , _cdnCertificateBody = pCertificateBody_
    , _cdnCertificatePrivateKey = pCertificatePrivateKey_
    , _cdnCertificateChain = pCertificateChain_
    }

-- | The name of the 'DomainName' resource.
cdnDomainName :: Lens' CreateDomainName Text
cdnDomainName = lens _cdnDomainName (\ s a -> s{_cdnDomainName = a});

-- | The name of the certificate.
cdnCertificateName :: Lens' CreateDomainName Text
cdnCertificateName = lens _cdnCertificateName (\ s a -> s{_cdnCertificateName = a});

-- | The body of the server certificate provided by your certificate authority.
cdnCertificateBody :: Lens' CreateDomainName Text
cdnCertificateBody = lens _cdnCertificateBody (\ s a -> s{_cdnCertificateBody = a});

-- | Your certificate's private key.
cdnCertificatePrivateKey :: Lens' CreateDomainName Text
cdnCertificatePrivateKey = lens _cdnCertificatePrivateKey (\ s a -> s{_cdnCertificatePrivateKey = a});

-- | The intermediate certificates and optionally the root certificate, one after the other without any blank lines. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
cdnCertificateChain :: Lens' CreateDomainName Text
cdnCertificateChain = lens _cdnCertificateChain (\ s a -> s{_cdnCertificateChain = a});

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
                 [Just ("domainName" .= _cdnDomainName),
                  Just ("certificateName" .= _cdnCertificateName),
                  Just ("certificateBody" .= _cdnCertificateBody),
                  Just
                    ("certificatePrivateKey" .=
                       _cdnCertificatePrivateKey),
                  Just ("certificateChain" .= _cdnCertificateChain)])

instance ToPath CreateDomainName where
        toPath = const "/domainnames"

instance ToQuery CreateDomainName where
        toQuery = const mempty
