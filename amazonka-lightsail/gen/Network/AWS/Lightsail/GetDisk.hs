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
-- Module      : Network.AWS.Lightsail.GetDisk
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific block storage disk.
--
--
module Network.AWS.Lightsail.GetDisk
    (
    -- * Creating a Request
      getDisk
    , GetDisk
    -- * Request Lenses
    , gdDiskName

    -- * Destructuring the Response
    , getDiskResponse
    , GetDiskResponse
    -- * Response Lenses
    , getrsDisk
    , getrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDisk' smart constructor.
newtype GetDisk = GetDisk'
  { _gdDiskName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDisk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdDiskName' - The name of the disk (e.g., @my-disk@ ).
getDisk
    :: Text -- ^ 'gdDiskName'
    -> GetDisk
getDisk pDiskName_ = GetDisk' {_gdDiskName = pDiskName_}


-- | The name of the disk (e.g., @my-disk@ ).
gdDiskName :: Lens' GetDisk Text
gdDiskName = lens _gdDiskName (\ s a -> s{_gdDiskName = a})

instance AWSRequest GetDisk where
        type Rs GetDisk = GetDiskResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetDiskResponse' <$>
                   (x .?> "disk") <*> (pure (fromEnum s)))

instance Hashable GetDisk where

instance NFData GetDisk where

instance ToHeaders GetDisk where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetDisk" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDisk where
        toJSON GetDisk'{..}
          = object
              (catMaybes [Just ("diskName" .= _gdDiskName)])

instance ToPath GetDisk where
        toPath = const "/"

instance ToQuery GetDisk where
        toQuery = const mempty

-- | /See:/ 'getDiskResponse' smart constructor.
data GetDiskResponse = GetDiskResponse'
  { _getrsDisk           :: !(Maybe Disk)
  , _getrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDiskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsDisk' - An object containing information about the disk.
--
-- * 'getrsResponseStatus' - -- | The response status code.
getDiskResponse
    :: Int -- ^ 'getrsResponseStatus'
    -> GetDiskResponse
getDiskResponse pResponseStatus_ =
  GetDiskResponse'
    {_getrsDisk = Nothing, _getrsResponseStatus = pResponseStatus_}


-- | An object containing information about the disk.
getrsDisk :: Lens' GetDiskResponse (Maybe Disk)
getrsDisk = lens _getrsDisk (\ s a -> s{_getrsDisk = a})

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetDiskResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\ s a -> s{_getrsResponseStatus = a})

instance NFData GetDiskResponse where
