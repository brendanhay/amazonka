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
-- Module      : Network.AWS.Lightsail.GetStaticIP
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific static IP.
--
--
module Network.AWS.Lightsail.GetStaticIP
    (
    -- * Creating a Request
      getStaticIP
    , GetStaticIP
    -- * Request Lenses
    , gsiStaticIPName

    -- * Destructuring the Response
    , getStaticIPResponse
    , GetStaticIPResponse
    -- * Response Lenses
    , gsirsStaticIP
    , gsirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getStaticIP' smart constructor.
newtype GetStaticIP = GetStaticIP'
  { _gsiStaticIPName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStaticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiStaticIPName' - The name of the static IP in Lightsail.
getStaticIP
    :: Text -- ^ 'gsiStaticIPName'
    -> GetStaticIP
getStaticIP pStaticIPName_ = GetStaticIP' {_gsiStaticIPName = pStaticIPName_}


-- | The name of the static IP in Lightsail.
gsiStaticIPName :: Lens' GetStaticIP Text
gsiStaticIPName = lens _gsiStaticIPName (\ s a -> s{_gsiStaticIPName = a})

instance AWSRequest GetStaticIP where
        type Rs GetStaticIP = GetStaticIPResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetStaticIPResponse' <$>
                   (x .?> "staticIp") <*> (pure (fromEnum s)))

instance Hashable GetStaticIP where

instance NFData GetStaticIP where

instance ToHeaders GetStaticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetStaticIp" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetStaticIP where
        toJSON GetStaticIP'{..}
          = object
              (catMaybes
                 [Just ("staticIpName" .= _gsiStaticIPName)])

instance ToPath GetStaticIP where
        toPath = const "/"

instance ToQuery GetStaticIP where
        toQuery = const mempty

-- | /See:/ 'getStaticIPResponse' smart constructor.
data GetStaticIPResponse = GetStaticIPResponse'
  { _gsirsStaticIP       :: !(Maybe StaticIP)
  , _gsirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStaticIPResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsirsStaticIP' - An array of key-value pairs containing information about the requested static IP.
--
-- * 'gsirsResponseStatus' - -- | The response status code.
getStaticIPResponse
    :: Int -- ^ 'gsirsResponseStatus'
    -> GetStaticIPResponse
getStaticIPResponse pResponseStatus_ =
  GetStaticIPResponse'
    {_gsirsStaticIP = Nothing, _gsirsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the requested static IP.
gsirsStaticIP :: Lens' GetStaticIPResponse (Maybe StaticIP)
gsirsStaticIP = lens _gsirsStaticIP (\ s a -> s{_gsirsStaticIP = a})

-- | -- | The response status code.
gsirsResponseStatus :: Lens' GetStaticIPResponse Int
gsirsResponseStatus = lens _gsirsResponseStatus (\ s a -> s{_gsirsResponseStatus = a})

instance NFData GetStaticIPResponse where
