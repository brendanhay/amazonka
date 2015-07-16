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
    , ldArn
    , ldNextToken

    -- * Response
    , ListDevicesResponse
    -- ** Response constructor
    , listDevicesResponse
    -- ** Response lenses
    , ldrNextToken
    , ldrDevices
    , ldrStatus
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
-- * 'ldArn'
--
-- * 'ldNextToken'
data ListDevices = ListDevices'
    { _ldArn       :: !(Maybe Text)
    , _ldNextToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDevices' smart constructor.
listDevices :: ListDevices
listDevices =
    ListDevices'
    { _ldArn = Nothing
    , _ldNextToken = Nothing
    }

-- | The device types\' ARNs.
ldArn :: Lens' ListDevices (Maybe Text)
ldArn = lens _ldArn (\ s a -> s{_ldArn = a});

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
ldNextToken :: Lens' ListDevices (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a});

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
              ["arn" .= _ldArn, "nextToken" .= _ldNextToken]

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
-- * 'ldrNextToken'
--
-- * 'ldrDevices'
--
-- * 'ldrStatus'
data ListDevicesResponse = ListDevicesResponse'
    { _ldrNextToken :: !(Maybe Text)
    , _ldrDevices   :: !(Maybe [Device])
    , _ldrStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDevicesResponse' smart constructor.
listDevicesResponse :: Int -> ListDevicesResponse
listDevicesResponse pStatus =
    ListDevicesResponse'
    { _ldrNextToken = Nothing
    , _ldrDevices = Nothing
    , _ldrStatus = pStatus
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
ldrNextToken :: Lens' ListDevicesResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\ s a -> s{_ldrNextToken = a});

-- | Information about the devices.
ldrDevices :: Lens' ListDevicesResponse [Device]
ldrDevices = lens _ldrDevices (\ s a -> s{_ldrDevices = a}) . _Default;

-- | FIXME: Undocumented member.
ldrStatus :: Lens' ListDevicesResponse Int
ldrStatus = lens _ldrStatus (\ s a -> s{_ldrStatus = a});
