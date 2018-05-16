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
-- Module      : Network.AWS.APIGateway.GenerateClientCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a 'ClientCertificate' resource.
--
--
module Network.AWS.APIGateway.GenerateClientCertificate
    (
    -- * Creating a Request
      generateClientCertificate
    , GenerateClientCertificate
    -- * Request Lenses
    , gccDescription

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

-- | A request to generate a 'ClientCertificate' resource.
--
--
--
-- /See:/ 'generateClientCertificate' smart constructor.
newtype GenerateClientCertificate = GenerateClientCertificate'
  { _gccDescription :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccDescription' - The description of the 'ClientCertificate' .
generateClientCertificate
    :: GenerateClientCertificate
generateClientCertificate =
  GenerateClientCertificate' {_gccDescription = Nothing}


-- | The description of the 'ClientCertificate' .
gccDescription :: Lens' GenerateClientCertificate (Maybe Text)
gccDescription = lens _gccDescription (\ s a -> s{_gccDescription = a})

instance AWSRequest GenerateClientCertificate where
        type Rs GenerateClientCertificate = ClientCertificate
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GenerateClientCertificate where

instance NFData GenerateClientCertificate where

instance ToHeaders GenerateClientCertificate where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON GenerateClientCertificate where
        toJSON GenerateClientCertificate'{..}
          = object
              (catMaybes [("description" .=) <$> _gccDescription])

instance ToPath GenerateClientCertificate where
        toPath = const "/clientcertificates"

instance ToQuery GenerateClientCertificate where
        toQuery = const mempty
