{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Resets a network interface attribute. You can specify only one attribute
-- at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetNetworkInterfaceAttribute.html>
module Network.AWS.EC2.ResetNetworkInterfaceAttribute
    (
    -- * Request
      ResetNetworkInterfaceAttribute
    -- ** Request constructor
    , resetNetworkInterfaceAttribute
    -- ** Request lenses
    , rniaSourceDestCheck
    , rniaDryRun
    , rniaNetworkInterfaceId

    -- * Response
    , ResetNetworkInterfaceAttributeResponse
    -- ** Response constructor
    , resetNetworkInterfaceAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'resetNetworkInterfaceAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rniaSourceDestCheck'
--
-- * 'rniaDryRun'
--
-- * 'rniaNetworkInterfaceId'
data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute'
    { _rniaSourceDestCheck    :: !(Maybe Text)
    , _rniaDryRun             :: !(Maybe Bool)
    , _rniaNetworkInterfaceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetNetworkInterfaceAttribute' smart constructor.
resetNetworkInterfaceAttribute :: Text -> ResetNetworkInterfaceAttribute
resetNetworkInterfaceAttribute pNetworkInterfaceId_ =
    ResetNetworkInterfaceAttribute'
    { _rniaSourceDestCheck = Nothing
    , _rniaDryRun = Nothing
    , _rniaNetworkInterfaceId = pNetworkInterfaceId_
    }

-- | The source\/destination checking attribute. Resets the value to @true@.
rniaSourceDestCheck :: Lens' ResetNetworkInterfaceAttribute (Maybe Text)
rniaSourceDestCheck = lens _rniaSourceDestCheck (\ s a -> s{_rniaSourceDestCheck = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rniaDryRun :: Lens' ResetNetworkInterfaceAttribute (Maybe Bool)
rniaDryRun = lens _rniaDryRun (\ s a -> s{_rniaDryRun = a});

-- | The ID of the network interface.
rniaNetworkInterfaceId :: Lens' ResetNetworkInterfaceAttribute Text
rniaNetworkInterfaceId = lens _rniaNetworkInterfaceId (\ s a -> s{_rniaNetworkInterfaceId = a});

instance AWSRequest ResetNetworkInterfaceAttribute
         where
        type Sv ResetNetworkInterfaceAttribute = EC2
        type Rs ResetNetworkInterfaceAttribute =
             ResetNetworkInterfaceAttributeResponse
        request = post
        response
          = receiveNull ResetNetworkInterfaceAttributeResponse'

instance ToHeaders ResetNetworkInterfaceAttribute
         where
        toHeaders = const mempty

instance ToPath ResetNetworkInterfaceAttribute where
        toPath = const "/"

instance ToQuery ResetNetworkInterfaceAttribute where
        toQuery ResetNetworkInterfaceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ResetNetworkInterfaceAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "SourceDestCheck" =: _rniaSourceDestCheck,
               "DryRun" =: _rniaDryRun,
               "NetworkInterfaceId" =: _rniaNetworkInterfaceId]

-- | /See:/ 'resetNetworkInterfaceAttributeResponse' smart constructor.
data ResetNetworkInterfaceAttributeResponse =
    ResetNetworkInterfaceAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetNetworkInterfaceAttributeResponse' smart constructor.
resetNetworkInterfaceAttributeResponse :: ResetNetworkInterfaceAttributeResponse
resetNetworkInterfaceAttributeResponse =
    ResetNetworkInterfaceAttributeResponse'
