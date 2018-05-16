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
-- Module      : Network.AWS.Lightsail.AttachDisk
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a block storage disk to a running or stopped Lightsail instance and exposes it to the instance with the specified disk name.
--
--
module Network.AWS.Lightsail.AttachDisk
    (
    -- * Creating a Request
      attachDisk
    , AttachDisk
    -- * Request Lenses
    , adDiskName
    , adInstanceName
    , adDiskPath

    -- * Destructuring the Response
    , attachDiskResponse
    , AttachDiskResponse
    -- * Response Lenses
    , adrsOperations
    , adrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachDisk' smart constructor.
data AttachDisk = AttachDisk'
  { _adDiskName     :: !Text
  , _adInstanceName :: !Text
  , _adDiskPath     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachDisk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adDiskName' - The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- * 'adInstanceName' - The name of the Lightsail instance where you want to utilize the storage disk.
--
-- * 'adDiskPath' - The disk path to expose to the instance (e.g., @/dev/xvdf@ ).
attachDisk
    :: Text -- ^ 'adDiskName'
    -> Text -- ^ 'adInstanceName'
    -> Text -- ^ 'adDiskPath'
    -> AttachDisk
attachDisk pDiskName_ pInstanceName_ pDiskPath_ =
  AttachDisk'
    { _adDiskName = pDiskName_
    , _adInstanceName = pInstanceName_
    , _adDiskPath = pDiskPath_
    }


-- | The unique Lightsail disk name (e.g., @my-disk@ ).
adDiskName :: Lens' AttachDisk Text
adDiskName = lens _adDiskName (\ s a -> s{_adDiskName = a})

-- | The name of the Lightsail instance where you want to utilize the storage disk.
adInstanceName :: Lens' AttachDisk Text
adInstanceName = lens _adInstanceName (\ s a -> s{_adInstanceName = a})

-- | The disk path to expose to the instance (e.g., @/dev/xvdf@ ).
adDiskPath :: Lens' AttachDisk Text
adDiskPath = lens _adDiskPath (\ s a -> s{_adDiskPath = a})

instance AWSRequest AttachDisk where
        type Rs AttachDisk = AttachDiskResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 AttachDiskResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable AttachDisk where

instance NFData AttachDisk where

instance ToHeaders AttachDisk where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.AttachDisk" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AttachDisk where
        toJSON AttachDisk'{..}
          = object
              (catMaybes
                 [Just ("diskName" .= _adDiskName),
                  Just ("instanceName" .= _adInstanceName),
                  Just ("diskPath" .= _adDiskPath)])

instance ToPath AttachDisk where
        toPath = const "/"

instance ToQuery AttachDisk where
        toQuery = const mempty

-- | /See:/ 'attachDiskResponse' smart constructor.
data AttachDiskResponse = AttachDiskResponse'
  { _adrsOperations     :: !(Maybe [Operation])
  , _adrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachDiskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adrsOperations' - An object describing the API operations.
--
-- * 'adrsResponseStatus' - -- | The response status code.
attachDiskResponse
    :: Int -- ^ 'adrsResponseStatus'
    -> AttachDiskResponse
attachDiskResponse pResponseStatus_ =
  AttachDiskResponse'
    {_adrsOperations = Nothing, _adrsResponseStatus = pResponseStatus_}


-- | An object describing the API operations.
adrsOperations :: Lens' AttachDiskResponse [Operation]
adrsOperations = lens _adrsOperations (\ s a -> s{_adrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
adrsResponseStatus :: Lens' AttachDiskResponse Int
adrsResponseStatus = lens _adrsResponseStatus (\ s a -> s{_adrsResponseStatus = a})

instance NFData AttachDiskResponse where
