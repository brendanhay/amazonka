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
-- Module      : Network.AWS.OpsWorksCM.UpdateServerEngineAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates engine-specific attributes on a specified server. The server enters the @MODIFYING@ state when this operation is in progress. Only one update can occur at a time. You can use this command to reset a Chef server's private key (@CHEF_PIVOTAL_KEY@ ), a Chef server's admin password (@CHEF_DELIVERY_ADMIN_PASSWORD@ ), or a Puppet server's admin password (@PUPPET_ADMIN_PASSWORD@ ).
--
--
-- This operation is asynchronous.
--
-- This operation can only be called for servers in @HEALTHY@ or @UNHEALTHY@ states. Otherwise, an @InvalidStateException@ is raised. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
module Network.AWS.OpsWorksCM.UpdateServerEngineAttributes
    (
    -- * Creating a Request
      updateServerEngineAttributes
    , UpdateServerEngineAttributes
    -- * Request Lenses
    , useaAttributeValue
    , useaServerName
    , useaAttributeName

    -- * Destructuring the Response
    , updateServerEngineAttributesResponse
    , UpdateServerEngineAttributesResponse
    -- * Response Lenses
    , usearsServer
    , usearsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types
import Network.AWS.OpsWorksCM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateServerEngineAttributes' smart constructor.
data UpdateServerEngineAttributes = UpdateServerEngineAttributes'
  { _useaAttributeValue :: !(Maybe Text)
  , _useaServerName     :: !Text
  , _useaAttributeName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateServerEngineAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'useaAttributeValue' - The value to set for the attribute.
--
-- * 'useaServerName' - The name of the server to update.
--
-- * 'useaAttributeName' - The name of the engine attribute to update.
updateServerEngineAttributes
    :: Text -- ^ 'useaServerName'
    -> Text -- ^ 'useaAttributeName'
    -> UpdateServerEngineAttributes
updateServerEngineAttributes pServerName_ pAttributeName_ =
  UpdateServerEngineAttributes'
    { _useaAttributeValue = Nothing
    , _useaServerName = pServerName_
    , _useaAttributeName = pAttributeName_
    }


-- | The value to set for the attribute.
useaAttributeValue :: Lens' UpdateServerEngineAttributes (Maybe Text)
useaAttributeValue = lens _useaAttributeValue (\ s a -> s{_useaAttributeValue = a})

-- | The name of the server to update.
useaServerName :: Lens' UpdateServerEngineAttributes Text
useaServerName = lens _useaServerName (\ s a -> s{_useaServerName = a})

-- | The name of the engine attribute to update.
useaAttributeName :: Lens' UpdateServerEngineAttributes Text
useaAttributeName = lens _useaAttributeName (\ s a -> s{_useaAttributeName = a})

instance AWSRequest UpdateServerEngineAttributes
         where
        type Rs UpdateServerEngineAttributes =
             UpdateServerEngineAttributesResponse
        request = postJSON opsWorksCM
        response
          = receiveJSON
              (\ s h x ->
                 UpdateServerEngineAttributesResponse' <$>
                   (x .?> "Server") <*> (pure (fromEnum s)))

instance Hashable UpdateServerEngineAttributes where

instance NFData UpdateServerEngineAttributes where

instance ToHeaders UpdateServerEngineAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.UpdateServerEngineAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateServerEngineAttributes where
        toJSON UpdateServerEngineAttributes'{..}
          = object
              (catMaybes
                 [("AttributeValue" .=) <$> _useaAttributeValue,
                  Just ("ServerName" .= _useaServerName),
                  Just ("AttributeName" .= _useaAttributeName)])

instance ToPath UpdateServerEngineAttributes where
        toPath = const "/"

instance ToQuery UpdateServerEngineAttributes where
        toQuery = const mempty

-- | /See:/ 'updateServerEngineAttributesResponse' smart constructor.
data UpdateServerEngineAttributesResponse = UpdateServerEngineAttributesResponse'
  { _usearsServer         :: !(Maybe Server)
  , _usearsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateServerEngineAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usearsServer' - Contains the response to an @UpdateServerEngineAttributes@ request.
--
-- * 'usearsResponseStatus' - -- | The response status code.
updateServerEngineAttributesResponse
    :: Int -- ^ 'usearsResponseStatus'
    -> UpdateServerEngineAttributesResponse
updateServerEngineAttributesResponse pResponseStatus_ =
  UpdateServerEngineAttributesResponse'
    {_usearsServer = Nothing, _usearsResponseStatus = pResponseStatus_}


-- | Contains the response to an @UpdateServerEngineAttributes@ request.
usearsServer :: Lens' UpdateServerEngineAttributesResponse (Maybe Server)
usearsServer = lens _usearsServer (\ s a -> s{_usearsServer = a})

-- | -- | The response status code.
usearsResponseStatus :: Lens' UpdateServerEngineAttributesResponse Int
usearsResponseStatus = lens _usearsResponseStatus (\ s a -> s{_usearsResponseStatus = a})

instance NFData UpdateServerEngineAttributesResponse
         where
