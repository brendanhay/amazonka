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
-- Module      : Network.AWS.EC2.ReleaseHosts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you no longer want to use an On-Demand Dedicated Host it can be released. On-Demand billing is stopped and the host goes into @released@ state. The host ID of Dedicated Hosts that have been released can no longer be specified in another request, for example, to modify the host. You must stop or terminate all instances on a host before it can be released.
--
--
-- When Dedicated Hosts are released, it may take some time for them to stop counting toward your limit and you may receive capacity errors when trying to allocate new Dedicated Hosts. Wait a few minutes and then try again.
--
-- Released hosts still appear in a 'DescribeHosts' response.
--
module Network.AWS.EC2.ReleaseHosts
    (
    -- * Creating a Request
      releaseHosts
    , ReleaseHosts
    -- * Request Lenses
    , rhHostIds

    -- * Destructuring the Response
    , releaseHostsResponse
    , ReleaseHostsResponse
    -- * Response Lenses
    , rhrsUnsuccessful
    , rhrsSuccessful
    , rhrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'releaseHosts' smart constructor.
newtype ReleaseHosts = ReleaseHosts'
  { _rhHostIds :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReleaseHosts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rhHostIds' - The IDs of the Dedicated Hosts to release.
releaseHosts
    :: ReleaseHosts
releaseHosts = ReleaseHosts' {_rhHostIds = mempty}


-- | The IDs of the Dedicated Hosts to release.
rhHostIds :: Lens' ReleaseHosts [Text]
rhHostIds = lens _rhHostIds (\ s a -> s{_rhHostIds = a}) . _Coerce

instance AWSRequest ReleaseHosts where
        type Rs ReleaseHosts = ReleaseHostsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ReleaseHostsResponse' <$>
                   (x .@? "unsuccessful" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*>
                     (x .@? "successful" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable ReleaseHosts where

instance NFData ReleaseHosts where

instance ToHeaders ReleaseHosts where
        toHeaders = const mempty

instance ToPath ReleaseHosts where
        toPath = const "/"

instance ToQuery ReleaseHosts where
        toQuery ReleaseHosts'{..}
          = mconcat
              ["Action" =: ("ReleaseHosts" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQueryList "HostId" _rhHostIds]

-- | /See:/ 'releaseHostsResponse' smart constructor.
data ReleaseHostsResponse = ReleaseHostsResponse'
  { _rhrsUnsuccessful   :: !(Maybe [UnsuccessfulItem])
  , _rhrsSuccessful     :: !(Maybe [Text])
  , _rhrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReleaseHostsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rhrsUnsuccessful' - The IDs of the Dedicated Hosts that could not be released, including an error message.
--
-- * 'rhrsSuccessful' - The IDs of the Dedicated Hosts that were successfully released.
--
-- * 'rhrsResponseStatus' - -- | The response status code.
releaseHostsResponse
    :: Int -- ^ 'rhrsResponseStatus'
    -> ReleaseHostsResponse
releaseHostsResponse pResponseStatus_ =
  ReleaseHostsResponse'
    { _rhrsUnsuccessful = Nothing
    , _rhrsSuccessful = Nothing
    , _rhrsResponseStatus = pResponseStatus_
    }


-- | The IDs of the Dedicated Hosts that could not be released, including an error message.
rhrsUnsuccessful :: Lens' ReleaseHostsResponse [UnsuccessfulItem]
rhrsUnsuccessful = lens _rhrsUnsuccessful (\ s a -> s{_rhrsUnsuccessful = a}) . _Default . _Coerce

-- | The IDs of the Dedicated Hosts that were successfully released.
rhrsSuccessful :: Lens' ReleaseHostsResponse [Text]
rhrsSuccessful = lens _rhrsSuccessful (\ s a -> s{_rhrsSuccessful = a}) . _Default . _Coerce

-- | -- | The response status code.
rhrsResponseStatus :: Lens' ReleaseHostsResponse Int
rhrsResponseStatus = lens _rhrsResponseStatus (\ s a -> s{_rhrsResponseStatus = a})

instance NFData ReleaseHostsResponse where
