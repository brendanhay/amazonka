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
-- Module      : Network.AWS.APIGateway.GetMethodResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a 'MethodResponse' resource.
--
--
module Network.AWS.APIGateway.GetMethodResponse
    (
    -- * Creating a Request
      getMethodResponse
    , GetMethodResponse
    -- * Request Lenses
    , gmRestAPIId
    , gmResourceId
    , gmHttpMethod
    , gmStatusCode

    -- * Destructuring the Response
    , methodResponse
    , MethodResponse
    -- * Response Lenses
    , mResponseModels
    , mStatusCode
    , mResponseParameters
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to describe a 'MethodResponse' resource.
--
--
--
-- /See:/ 'getMethodResponse' smart constructor.
data GetMethodResponse = GetMethodResponse'
  { _gmRestAPIId  :: !Text
  , _gmResourceId :: !Text
  , _gmHttpMethod :: !Text
  , _gmStatusCode :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'gmResourceId' - [Required] The 'Resource' identifier for the 'MethodResponse' resource.
--
-- * 'gmHttpMethod' - [Required] The HTTP verb of the 'Method' resource.
--
-- * 'gmStatusCode' - [Required] The status code for the 'MethodResponse' resource.
getMethodResponse
    :: Text -- ^ 'gmRestAPIId'
    -> Text -- ^ 'gmResourceId'
    -> Text -- ^ 'gmHttpMethod'
    -> Text -- ^ 'gmStatusCode'
    -> GetMethodResponse
getMethodResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
  GetMethodResponse'
    { _gmRestAPIId = pRestAPIId_
    , _gmResourceId = pResourceId_
    , _gmHttpMethod = pHttpMethod_
    , _gmStatusCode = pStatusCode_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
gmRestAPIId :: Lens' GetMethodResponse Text
gmRestAPIId = lens _gmRestAPIId (\ s a -> s{_gmRestAPIId = a})

-- | [Required] The 'Resource' identifier for the 'MethodResponse' resource.
gmResourceId :: Lens' GetMethodResponse Text
gmResourceId = lens _gmResourceId (\ s a -> s{_gmResourceId = a})

-- | [Required] The HTTP verb of the 'Method' resource.
gmHttpMethod :: Lens' GetMethodResponse Text
gmHttpMethod = lens _gmHttpMethod (\ s a -> s{_gmHttpMethod = a})

-- | [Required] The status code for the 'MethodResponse' resource.
gmStatusCode :: Lens' GetMethodResponse Text
gmStatusCode = lens _gmStatusCode (\ s a -> s{_gmStatusCode = a})

instance AWSRequest GetMethodResponse where
        type Rs GetMethodResponse = MethodResponse
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetMethodResponse where

instance NFData GetMethodResponse where

instance ToHeaders GetMethodResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetMethodResponse where
        toPath GetMethodResponse'{..}
          = mconcat
              ["/restapis/", toBS _gmRestAPIId, "/resources/",
               toBS _gmResourceId, "/methods/", toBS _gmHttpMethod,
               "/responses/", toBS _gmStatusCode]

instance ToQuery GetMethodResponse where
        toQuery = const mempty
