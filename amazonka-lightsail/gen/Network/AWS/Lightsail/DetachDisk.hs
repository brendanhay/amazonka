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
-- Module      : Network.AWS.Lightsail.DetachDisk
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a stopped block storage disk from a Lightsail instance. Make sure to unmount any file systems on the device within your operating system before stopping the instance and detaching the disk.
--
--
module Network.AWS.Lightsail.DetachDisk
    (
    -- * Creating a Request
      detachDisk
    , DetachDisk
    -- * Request Lenses
    , ddDiskName

    -- * Destructuring the Response
    , detachDiskResponse
    , DetachDiskResponse
    -- * Response Lenses
    , ddrsOperations
    , ddrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachDisk' smart constructor.
newtype DetachDisk = DetachDisk'
  { _ddDiskName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachDisk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDiskName' - The unique name of the disk you want to detach from your instance (e.g., @my-disk@ ).
detachDisk
    :: Text -- ^ 'ddDiskName'
    -> DetachDisk
detachDisk pDiskName_ = DetachDisk' {_ddDiskName = pDiskName_}


-- | The unique name of the disk you want to detach from your instance (e.g., @my-disk@ ).
ddDiskName :: Lens' DetachDisk Text
ddDiskName = lens _ddDiskName (\ s a -> s{_ddDiskName = a})

instance AWSRequest DetachDisk where
        type Rs DetachDisk = DetachDiskResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DetachDiskResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DetachDisk where

instance NFData DetachDisk where

instance ToHeaders DetachDisk where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DetachDisk" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetachDisk where
        toJSON DetachDisk'{..}
          = object
              (catMaybes [Just ("diskName" .= _ddDiskName)])

instance ToPath DetachDisk where
        toPath = const "/"

instance ToQuery DetachDisk where
        toQuery = const mempty

-- | /See:/ 'detachDiskResponse' smart constructor.
data DetachDiskResponse = DetachDiskResponse'
  { _ddrsOperations     :: !(Maybe [Operation])
  , _ddrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachDiskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsOperations' - An object describing the API operations.
--
-- * 'ddrsResponseStatus' - -- | The response status code.
detachDiskResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DetachDiskResponse
detachDiskResponse pResponseStatus_ =
  DetachDiskResponse'
    {_ddrsOperations = Nothing, _ddrsResponseStatus = pResponseStatus_}


-- | An object describing the API operations.
ddrsOperations :: Lens' DetachDiskResponse [Operation]
ddrsOperations = lens _ddrsOperations (\ s a -> s{_ddrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DetachDiskResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DetachDiskResponse where
