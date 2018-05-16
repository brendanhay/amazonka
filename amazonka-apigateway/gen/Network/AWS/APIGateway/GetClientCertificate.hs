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
-- Module      : Network.AWS.APIGateway.GetClientCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current 'ClientCertificate' resource.
--
--
module Network.AWS.APIGateway.GetClientCertificate
    (
    -- * Creating a Request
      getClientCertificate
    , GetClientCertificate
    -- * Request Lenses
    , gccClientCertificateId

    -- * Destructuring the Response
    , clientCertificate
    , ClientCertificate
    -- * Response Lenses
    , ccPemEncodedCertificate
    , ccClientCertificateId
    , ccCreatedDate
    , ccExpirationDate
    , ccDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to get information about the current 'ClientCertificate' resource.
--
--
--
-- /See:/ 'getClientCertificate' smart constructor.
newtype GetClientCertificate = GetClientCertificate'
  { _gccClientCertificateId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccClientCertificateId' - [Required] The identifier of the 'ClientCertificate' resource to be described.
getClientCertificate
    :: Text -- ^ 'gccClientCertificateId'
    -> GetClientCertificate
getClientCertificate pClientCertificateId_ =
  GetClientCertificate' {_gccClientCertificateId = pClientCertificateId_}


-- | [Required] The identifier of the 'ClientCertificate' resource to be described.
gccClientCertificateId :: Lens' GetClientCertificate Text
gccClientCertificateId = lens _gccClientCertificateId (\ s a -> s{_gccClientCertificateId = a})

instance AWSRequest GetClientCertificate where
        type Rs GetClientCertificate = ClientCertificate
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetClientCertificate where

instance NFData GetClientCertificate where

instance ToHeaders GetClientCertificate where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetClientCertificate where
        toPath GetClientCertificate'{..}
          = mconcat
              ["/clientcertificates/",
               toBS _gccClientCertificateId]

instance ToQuery GetClientCertificate where
        toQuery = const mempty
