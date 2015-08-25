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
-- Module      : Network.AWS.DeviceFarm.ListDevices
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about unique device types.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListDevices.html AWS API Reference> for ListDevices.
module Network.AWS.DeviceFarm.ListDevices
    (
    -- * Creating a Request
      listDevices
    , ListDevices
    -- * Request Lenses
    , ldArn
    , ldNextToken

    -- * Destructuring the Response
    , listDevicesResponse
    , ListDevicesResponse
    -- * Response Lenses
    , ldrsNextToken
    , ldrsDevices
    , ldrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.DeviceFarm.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the result of a list devices request.
--
-- /See:/ 'listDevices' smart constructor.
data ListDevices = ListDevices'
    { _ldArn       :: !(Maybe Text)
    , _ldNextToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDevices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldArn'
--
-- * 'ldNextToken'
listDevices
    :: ListDevices
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
        type Rs ListDevices = ListDevicesResponse
        request = postJSON deviceFarm
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
              (catMaybes
                 [("arn" .=) <$> _ldArn,
                  ("nextToken" .=) <$> _ldNextToken])

instance ToPath ListDevices where
        toPath = const "/"

instance ToQuery ListDevices where
        toQuery = const mempty

-- | Represents the result of a list devices operation.
--
-- /See:/ 'listDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
    { _ldrsNextToken :: !(Maybe Text)
    , _ldrsDevices   :: !(Maybe [Device])
    , _ldrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDevicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsNextToken'
--
-- * 'ldrsDevices'
--
-- * 'ldrsStatus'
listDevicesResponse
    :: Int -- ^ 'ldrsStatus'
    -> ListDevicesResponse
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
ldrsDevices = lens _ldrsDevices (\ s a -> s{_ldrsDevices = a}) . _Default . _Coerce;

-- | The response status code.
ldrsStatus :: Lens' ListDevicesResponse Int
ldrsStatus = lens _ldrsStatus (\ s a -> s{_ldrsStatus = a});
