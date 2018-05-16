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
-- Module      : Network.AWS.DeviceFarm.CreateDevicePool
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device pool.
--
--
module Network.AWS.DeviceFarm.CreateDevicePool
    (
    -- * Creating a Request
      createDevicePool
    , CreateDevicePool
    -- * Request Lenses
    , cdpDescription
    , cdpProjectARN
    , cdpName
    , cdpRules

    -- * Destructuring the Response
    , createDevicePoolResponse
    , CreateDevicePoolResponse
    -- * Response Lenses
    , cdprsDevicePool
    , cdprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the create device pool operation.
--
--
--
-- /See:/ 'createDevicePool' smart constructor.
data CreateDevicePool = CreateDevicePool'
  { _cdpDescription :: !(Maybe Text)
  , _cdpProjectARN  :: !Text
  , _cdpName        :: !Text
  , _cdpRules       :: ![Rule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDevicePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdpDescription' - The device pool's description.
--
-- * 'cdpProjectARN' - The ARN of the project for the device pool.
--
-- * 'cdpName' - The device pool's name.
--
-- * 'cdpRules' - The device pool's rules.
createDevicePool
    :: Text -- ^ 'cdpProjectARN'
    -> Text -- ^ 'cdpName'
    -> CreateDevicePool
createDevicePool pProjectARN_ pName_ =
  CreateDevicePool'
    { _cdpDescription = Nothing
    , _cdpProjectARN = pProjectARN_
    , _cdpName = pName_
    , _cdpRules = mempty
    }


-- | The device pool's description.
cdpDescription :: Lens' CreateDevicePool (Maybe Text)
cdpDescription = lens _cdpDescription (\ s a -> s{_cdpDescription = a})

-- | The ARN of the project for the device pool.
cdpProjectARN :: Lens' CreateDevicePool Text
cdpProjectARN = lens _cdpProjectARN (\ s a -> s{_cdpProjectARN = a})

-- | The device pool's name.
cdpName :: Lens' CreateDevicePool Text
cdpName = lens _cdpName (\ s a -> s{_cdpName = a})

-- | The device pool's rules.
cdpRules :: Lens' CreateDevicePool [Rule]
cdpRules = lens _cdpRules (\ s a -> s{_cdpRules = a}) . _Coerce

instance AWSRequest CreateDevicePool where
        type Rs CreateDevicePool = CreateDevicePoolResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 CreateDevicePoolResponse' <$>
                   (x .?> "devicePool") <*> (pure (fromEnum s)))

instance Hashable CreateDevicePool where

instance NFData CreateDevicePool where

instance ToHeaders CreateDevicePool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.CreateDevicePool" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDevicePool where
        toJSON CreateDevicePool'{..}
          = object
              (catMaybes
                 [("description" .=) <$> _cdpDescription,
                  Just ("projectArn" .= _cdpProjectARN),
                  Just ("name" .= _cdpName),
                  Just ("rules" .= _cdpRules)])

instance ToPath CreateDevicePool where
        toPath = const "/"

instance ToQuery CreateDevicePool where
        toQuery = const mempty

-- | Represents the result of a create device pool request.
--
--
--
-- /See:/ 'createDevicePoolResponse' smart constructor.
data CreateDevicePoolResponse = CreateDevicePoolResponse'
  { _cdprsDevicePool     :: !(Maybe DevicePool)
  , _cdprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDevicePoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdprsDevicePool' - The newly created device pool.
--
-- * 'cdprsResponseStatus' - -- | The response status code.
createDevicePoolResponse
    :: Int -- ^ 'cdprsResponseStatus'
    -> CreateDevicePoolResponse
createDevicePoolResponse pResponseStatus_ =
  CreateDevicePoolResponse'
    {_cdprsDevicePool = Nothing, _cdprsResponseStatus = pResponseStatus_}


-- | The newly created device pool.
cdprsDevicePool :: Lens' CreateDevicePoolResponse (Maybe DevicePool)
cdprsDevicePool = lens _cdprsDevicePool (\ s a -> s{_cdprsDevicePool = a})

-- | -- | The response status code.
cdprsResponseStatus :: Lens' CreateDevicePoolResponse Int
cdprsResponseStatus = lens _cdprsResponseStatus (\ s a -> s{_cdprsResponseStatus = a})

instance NFData CreateDevicePoolResponse where
