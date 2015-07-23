{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListDevices
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about unique device types.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListDevices.html>
module Network.AWS.DeviceFarm.ListDevices
    (
    -- * Request
      ListDevices
    -- ** Request constructor
    , listDevices
    -- ** Request lenses
    , ldrqArn
    , ldrqNextToken

    -- * Response
    , ListDevicesResponse
    -- ** Response constructor
    , listDevicesResponse
    -- ** Response lenses
    , ldrsNextToken
    , ldrsDevices
    , ldrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the result of a list devices request.
--
-- /See:/ 'listDevices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrqArn'
--
-- * 'ldrqNextToken'
data ListDevices = ListDevices'
    { _ldrqArn       :: !(Maybe Text)
    , _ldrqNextToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDevices' smart constructor.
listDevices :: ListDevices
listDevices =
    ListDevices'
    { _ldrqArn = Nothing
    , _ldrqNextToken = Nothing
    }

-- | The device types\' ARNs.
ldrqArn :: Lens' ListDevices (Maybe Text)
ldrqArn = lens _ldrqArn (\ s a -> s{_ldrqArn = a});

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
ldrqNextToken :: Lens' ListDevices (Maybe Text)
ldrqNextToken = lens _ldrqNextToken (\ s a -> s{_ldrqNextToken = a});

instance AWSRequest ListDevices where
        type Sv ListDevices = DeviceFarm
        type Rs ListDevices = ListDevicesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDevicesResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "devices" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListDevices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListDevices" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDevices where
        toJSON ListDevices'{..}
          = object
              ["arn" .= _ldrqArn, "nextToken" .= _ldrqNextToken]

instance ToPath ListDevices where
        toPath = const "/"

instance ToQuery ListDevices where
        toQuery = const mempty

-- | Represents the result of a list devices operation.
--
-- /See:/ 'listDevicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrsNextToken'
--
-- * 'ldrsDevices'
--
-- * 'ldrsStatus'
data ListDevicesResponse = ListDevicesResponse'
    { _ldrsNextToken :: !(Maybe Text)
    , _ldrsDevices   :: !(Maybe [Device])
    , _ldrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDevicesResponse' smart constructor.
listDevicesResponse :: Int -> ListDevicesResponse
listDevicesResponse pStatus_ =
    ListDevicesResponse'
    { _ldrsNextToken = Nothing
    , _ldrsDevices = Nothing
    , _ldrsStatus = pStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
ldrsNextToken :: Lens' ListDevicesResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a});

-- | Information about the devices.
ldrsDevices :: Lens' ListDevicesResponse [Device]
ldrsDevices = lens _ldrsDevices (\ s a -> s{_ldrsDevices = a}) . _Default;

-- | FIXME: Undocumented member.
ldrsStatus :: Lens' ListDevicesResponse Int
ldrsStatus = lens _ldrsStatus (\ s a -> s{_ldrsStatus = a});
