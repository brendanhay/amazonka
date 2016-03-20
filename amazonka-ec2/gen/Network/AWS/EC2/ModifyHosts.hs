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
-- Module      : Network.AWS.EC2.ModifyHosts
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the auto-placement setting of a Dedicated host. When
-- auto-placement is enabled, AWS will place instances that you launch with
-- a tenancy of 'host', but without targeting a specific host ID, onto any
-- available Dedicated host in your account which has auto-placement
-- enabled. When auto-placement is disabled, you need to provide a host ID
-- if you want the instance to launch onto a specific host. If no host ID
-- is provided, the instance will be launched onto a suitable host which
-- has auto-placement enabled.
module Network.AWS.EC2.ModifyHosts
    (
    -- * Creating a Request
      modifyHosts
    , ModifyHosts
    -- * Request Lenses
    , mhHostIds
    , mhAutoPlacement

    -- * Destructuring the Response
    , modifyHostsResponse
    , ModifyHostsResponse
    -- * Response Lenses
    , mhrsUnsuccessful
    , mhrsSuccessful
    , mhrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyHosts' smart constructor.
data ModifyHosts = ModifyHosts'
    { _mhHostIds       :: ![Text]
    , _mhAutoPlacement :: !AutoPlacement
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyHosts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mhHostIds'
--
-- * 'mhAutoPlacement'
modifyHosts
    :: AutoPlacement -- ^ 'mhAutoPlacement'
    -> ModifyHosts
modifyHosts pAutoPlacement_ =
    ModifyHosts'
    { _mhHostIds = mempty
    , _mhAutoPlacement = pAutoPlacement_
    }

-- | The host IDs of the Dedicated hosts you want to modify.
mhHostIds :: Lens' ModifyHosts [Text]
mhHostIds = lens _mhHostIds (\ s a -> s{_mhHostIds = a}) . _Coerce;

-- | Specify whether to enable or disable auto-placement.
mhAutoPlacement :: Lens' ModifyHosts AutoPlacement
mhAutoPlacement = lens _mhAutoPlacement (\ s a -> s{_mhAutoPlacement = a});

instance AWSRequest ModifyHosts where
        type Rs ModifyHosts = ModifyHostsResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 ModifyHostsResponse' <$>
                   (x .@? "unsuccessful" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*>
                     (x .@? "successful" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable ModifyHosts

instance ToHeaders ModifyHosts where
        toHeaders = const mempty

instance ToPath ModifyHosts where
        toPath = const "/"

instance ToQuery ModifyHosts where
        toQuery ModifyHosts'{..}
          = mconcat
              ["Action" =: ("ModifyHosts" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               toQueryList "HostId" _mhHostIds,
               "AutoPlacement" =: _mhAutoPlacement]

-- | /See:/ 'modifyHostsResponse' smart constructor.
data ModifyHostsResponse = ModifyHostsResponse'
    { _mhrsUnsuccessful   :: !(Maybe [UnsuccessfulItem])
    , _mhrsSuccessful     :: !(Maybe [Text])
    , _mhrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyHostsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mhrsUnsuccessful'
--
-- * 'mhrsSuccessful'
--
-- * 'mhrsResponseStatus'
modifyHostsResponse
    :: Int -- ^ 'mhrsResponseStatus'
    -> ModifyHostsResponse
modifyHostsResponse pResponseStatus_ =
    ModifyHostsResponse'
    { _mhrsUnsuccessful = Nothing
    , _mhrsSuccessful = Nothing
    , _mhrsResponseStatus = pResponseStatus_
    }

-- | The IDs of the Dedicated hosts that could not be modified. Check whether
-- the setting you requested can be used.
mhrsUnsuccessful :: Lens' ModifyHostsResponse [UnsuccessfulItem]
mhrsUnsuccessful = lens _mhrsUnsuccessful (\ s a -> s{_mhrsUnsuccessful = a}) . _Default . _Coerce;

-- | The IDs of the Dedicated hosts that were successfully modified.
mhrsSuccessful :: Lens' ModifyHostsResponse [Text]
mhrsSuccessful = lens _mhrsSuccessful (\ s a -> s{_mhrsSuccessful = a}) . _Default . _Coerce;

-- | The response status code.
mhrsResponseStatus :: Lens' ModifyHostsResponse Int
mhrsResponseStatus = lens _mhrsResponseStatus (\ s a -> s{_mhrsResponseStatus = a});
