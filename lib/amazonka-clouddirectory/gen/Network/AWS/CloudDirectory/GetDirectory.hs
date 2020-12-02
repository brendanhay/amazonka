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
-- Module      : Network.AWS.CloudDirectory.GetDirectory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about a directory.
--
--
module Network.AWS.CloudDirectory.GetDirectory
    (
    -- * Creating a Request
      getDirectory
    , GetDirectory
    -- * Request Lenses
    , gdDirectoryARN

    -- * Destructuring the Response
    , getDirectoryResponse
    , GetDirectoryResponse
    -- * Response Lenses
    , gdrsResponseStatus
    , gdrsDirectory
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDirectory' smart constructor.
newtype GetDirectory = GetDirectory'
  { _gdDirectoryARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdDirectoryARN' - The ARN of the directory.
getDirectory
    :: Text -- ^ 'gdDirectoryARN'
    -> GetDirectory
getDirectory pDirectoryARN_ = GetDirectory' {_gdDirectoryARN = pDirectoryARN_}


-- | The ARN of the directory.
gdDirectoryARN :: Lens' GetDirectory Text
gdDirectoryARN = lens _gdDirectoryARN (\ s a -> s{_gdDirectoryARN = a})

instance AWSRequest GetDirectory where
        type Rs GetDirectory = GetDirectoryResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 GetDirectoryResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Directory"))

instance Hashable GetDirectory where

instance NFData GetDirectory where

instance ToHeaders GetDirectory where
        toHeaders GetDirectory'{..}
          = mconcat ["x-amz-data-partition" =# _gdDirectoryARN]

instance ToJSON GetDirectory where
        toJSON = const (Object mempty)

instance ToPath GetDirectory where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/directory/get"

instance ToQuery GetDirectory where
        toQuery = const mempty

-- | /See:/ 'getDirectoryResponse' smart constructor.
data GetDirectoryResponse = GetDirectoryResponse'
  { _gdrsResponseStatus :: !Int
  , _gdrsDirectory      :: !Directory
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsResponseStatus' - -- | The response status code.
--
-- * 'gdrsDirectory' - Metadata about the directory.
getDirectoryResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> Directory -- ^ 'gdrsDirectory'
    -> GetDirectoryResponse
getDirectoryResponse pResponseStatus_ pDirectory_ =
  GetDirectoryResponse'
    {_gdrsResponseStatus = pResponseStatus_, _gdrsDirectory = pDirectory_}


-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDirectoryResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

-- | Metadata about the directory.
gdrsDirectory :: Lens' GetDirectoryResponse Directory
gdrsDirectory = lens _gdrsDirectory (\ s a -> s{_gdrsDirectory = a})

instance NFData GetDirectoryResponse where
