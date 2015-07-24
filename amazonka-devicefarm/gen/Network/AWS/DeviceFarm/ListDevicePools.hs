{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListDevicePools
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about device pools.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListDevicePools.html>
module Network.AWS.DeviceFarm.ListDevicePools
    (
    -- * Request
      ListDevicePools
    -- ** Request constructor
    , listDevicePools
    -- ** Request lenses
    , ldpNextToken
    , ldpType
    , ldpArn

    -- * Response
    , ListDevicePoolsResponse
    -- ** Response constructor
    , listDevicePoolsResponse
    -- ** Response lenses
    , ldprsDevicePools
    , ldprsNextToken
    , ldprsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the result of a list device pools request.
--
-- /See:/ 'listDevicePools' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldpNextToken'
--
-- * 'ldpType'
--
-- * 'ldpArn'
data ListDevicePools = ListDevicePools'
    { _ldpNextToken :: !(Maybe Text)
    , _ldpType      :: !(Maybe DevicePoolType)
    , _ldpArn       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDevicePools' smart constructor.
listDevicePools :: Text -> ListDevicePools
listDevicePools pArn_ =
    ListDevicePools'
    { _ldpNextToken = Nothing
    , _ldpType = Nothing
    , _ldpArn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
ldpNextToken :: Lens' ListDevicePools (Maybe Text)
ldpNextToken = lens _ldpNextToken (\ s a -> s{_ldpNextToken = a});

-- | The device pools\' type.
--
-- Allowed values include:
--
-- -   CURATED: A device pool that is created and managed by AWS Device
--     Farm.
--
-- -   PRIVATE: A device pool that is created and managed by the device
--     pool developer.
--
ldpType :: Lens' ListDevicePools (Maybe DevicePoolType)
ldpType = lens _ldpType (\ s a -> s{_ldpType = a});

-- | The project ARN.
ldpArn :: Lens' ListDevicePools Text
ldpArn = lens _ldpArn (\ s a -> s{_ldpArn = a});

instance AWSRequest ListDevicePools where
        type Sv ListDevicePools = DeviceFarm
        type Rs ListDevicePools = ListDevicePoolsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDevicePoolsResponse' <$>
                   (x .?> "devicePools" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListDevicePools where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListDevicePools" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDevicePools where
        toJSON ListDevicePools'{..}
          = object
              ["nextToken" .= _ldpNextToken, "type" .= _ldpType,
               "arn" .= _ldpArn]

instance ToPath ListDevicePools where
        toPath = const "/"

instance ToQuery ListDevicePools where
        toQuery = const mempty

-- | Represents the result of a list device pools request.
--
-- /See:/ 'listDevicePoolsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldprsDevicePools'
--
-- * 'ldprsNextToken'
--
-- * 'ldprsStatus'
data ListDevicePoolsResponse = ListDevicePoolsResponse'
    { _ldprsDevicePools :: !(Maybe [DevicePool])
    , _ldprsNextToken   :: !(Maybe Text)
    , _ldprsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDevicePoolsResponse' smart constructor.
listDevicePoolsResponse :: Int -> ListDevicePoolsResponse
listDevicePoolsResponse pStatus_ =
    ListDevicePoolsResponse'
    { _ldprsDevicePools = Nothing
    , _ldprsNextToken = Nothing
    , _ldprsStatus = pStatus_
    }

-- | Information about the device pools.
ldprsDevicePools :: Lens' ListDevicePoolsResponse [DevicePool]
ldprsDevicePools = lens _ldprsDevicePools (\ s a -> s{_ldprsDevicePools = a}) . _Default;

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
ldprsNextToken :: Lens' ListDevicePoolsResponse (Maybe Text)
ldprsNextToken = lens _ldprsNextToken (\ s a -> s{_ldprsNextToken = a});

-- | FIXME: Undocumented member.
ldprsStatus :: Lens' ListDevicePoolsResponse Int
ldprsStatus = lens _ldprsStatus (\ s a -> s{_ldprsStatus = a});
