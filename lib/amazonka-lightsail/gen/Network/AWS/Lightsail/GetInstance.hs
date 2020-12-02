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
-- Module      : Network.AWS.Lightsail.GetInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Amazon Lightsail instance, which is a virtual private server.
--
--
module Network.AWS.Lightsail.GetInstance
    (
    -- * Creating a Request
      getInstance
    , GetInstance
    -- * Request Lenses
    , giInstanceName

    -- * Destructuring the Response
    , getInstanceResponse
    , GetInstanceResponse
    -- * Response Lenses
    , girsInstance
    , girsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstance' smart constructor.
newtype GetInstance = GetInstance'
  { _giInstanceName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giInstanceName' - The name of the instance.
getInstance
    :: Text -- ^ 'giInstanceName'
    -> GetInstance
getInstance pInstanceName_ = GetInstance' {_giInstanceName = pInstanceName_}


-- | The name of the instance.
giInstanceName :: Lens' GetInstance Text
giInstanceName = lens _giInstanceName (\ s a -> s{_giInstanceName = a})

instance AWSRequest GetInstance where
        type Rs GetInstance = GetInstanceResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetInstanceResponse' <$>
                   (x .?> "instance") <*> (pure (fromEnum s)))

instance Hashable GetInstance where

instance NFData GetInstance where

instance ToHeaders GetInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstance where
        toJSON GetInstance'{..}
          = object
              (catMaybes
                 [Just ("instanceName" .= _giInstanceName)])

instance ToPath GetInstance where
        toPath = const "/"

instance ToQuery GetInstance where
        toQuery = const mempty

-- | /See:/ 'getInstanceResponse' smart constructor.
data GetInstanceResponse = GetInstanceResponse'
  { _girsInstance       :: !(Maybe Instance)
  , _girsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girsInstance' - An array of key-value pairs containing information about the specified instance.
--
-- * 'girsResponseStatus' - -- | The response status code.
getInstanceResponse
    :: Int -- ^ 'girsResponseStatus'
    -> GetInstanceResponse
getInstanceResponse pResponseStatus_ =
  GetInstanceResponse'
    {_girsInstance = Nothing, _girsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the specified instance.
girsInstance :: Lens' GetInstanceResponse (Maybe Instance)
girsInstance = lens _girsInstance (\ s a -> s{_girsInstance = a})

-- | -- | The response status code.
girsResponseStatus :: Lens' GetInstanceResponse Int
girsResponseStatus = lens _girsResponseStatus (\ s a -> s{_girsResponseStatus = a})

instance NFData GetInstanceResponse where
