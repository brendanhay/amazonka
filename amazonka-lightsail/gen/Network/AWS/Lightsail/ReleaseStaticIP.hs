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
-- Module      : Network.AWS.Lightsail.ReleaseStaticIP
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific static IP from your account.
--
--
module Network.AWS.Lightsail.ReleaseStaticIP
    (
    -- * Creating a Request
      releaseStaticIP
    , ReleaseStaticIP
    -- * Request Lenses
    , rsiStaticIPName

    -- * Destructuring the Response
    , releaseStaticIPResponse
    , ReleaseStaticIPResponse
    -- * Response Lenses
    , rsirsOperations
    , rsirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'releaseStaticIP' smart constructor.
newtype ReleaseStaticIP = ReleaseStaticIP'
  { _rsiStaticIPName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReleaseStaticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsiStaticIPName' - The name of the static IP to delete.
releaseStaticIP
    :: Text -- ^ 'rsiStaticIPName'
    -> ReleaseStaticIP
releaseStaticIP pStaticIPName_ =
  ReleaseStaticIP' {_rsiStaticIPName = pStaticIPName_}


-- | The name of the static IP to delete.
rsiStaticIPName :: Lens' ReleaseStaticIP Text
rsiStaticIPName = lens _rsiStaticIPName (\ s a -> s{_rsiStaticIPName = a})

instance AWSRequest ReleaseStaticIP where
        type Rs ReleaseStaticIP = ReleaseStaticIPResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 ReleaseStaticIPResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ReleaseStaticIP where

instance NFData ReleaseStaticIP where

instance ToHeaders ReleaseStaticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.ReleaseStaticIp" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ReleaseStaticIP where
        toJSON ReleaseStaticIP'{..}
          = object
              (catMaybes
                 [Just ("staticIpName" .= _rsiStaticIPName)])

instance ToPath ReleaseStaticIP where
        toPath = const "/"

instance ToQuery ReleaseStaticIP where
        toQuery = const mempty

-- | /See:/ 'releaseStaticIPResponse' smart constructor.
data ReleaseStaticIPResponse = ReleaseStaticIPResponse'
  { _rsirsOperations     :: !(Maybe [Operation])
  , _rsirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReleaseStaticIPResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsirsOperations' - An array of key-value pairs containing information about the request operation.
--
-- * 'rsirsResponseStatus' - -- | The response status code.
releaseStaticIPResponse
    :: Int -- ^ 'rsirsResponseStatus'
    -> ReleaseStaticIPResponse
releaseStaticIPResponse pResponseStatus_ =
  ReleaseStaticIPResponse'
    {_rsirsOperations = Nothing, _rsirsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the request operation.
rsirsOperations :: Lens' ReleaseStaticIPResponse [Operation]
rsirsOperations = lens _rsirsOperations (\ s a -> s{_rsirsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
rsirsResponseStatus :: Lens' ReleaseStaticIPResponse Int
rsirsResponseStatus = lens _rsirsResponseStatus (\ s a -> s{_rsirsResponseStatus = a})

instance NFData ReleaseStaticIPResponse where
