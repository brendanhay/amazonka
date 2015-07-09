{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.ModifyLoadBalancerAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Modifies the attributes of the specified load balancer.
--
-- You can modify the load balancer attributes, such as @AccessLogs@,
-- @ConnectionDraining@, and @CrossZoneLoadBalancing@ by either enabling or
-- disabling them. Or, you can modify the load balancer attribute
-- @ConnectionSettings@ by specifying an idle connection timeout value for
-- your load balancer.
--
-- For more information, see the following in the /Elastic Load Balancing
-- Developer Guide/:
--
-- -   <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#request-routing Cross-Zone Load Balancing>
-- -   <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
-- -   <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/access-log-collection.html Access Logs>
-- -   <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#idle-timeout Idle Connection Timeout>
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ModifyLoadBalancerAttributes.html>
module Network.AWS.ELB.ModifyLoadBalancerAttributes
    (
    -- * Request
      ModifyLoadBalancerAttributes
    -- ** Request constructor
    , modifyLoadBalancerAttributes
    -- ** Request lenses
    , mlbaLoadBalancerName
    , mlbaLoadBalancerAttributes

    -- * Response
    , ModifyLoadBalancerAttributesResponse
    -- ** Response constructor
    , modifyLoadBalancerAttributesResponse
    -- ** Response lenses
    , mlbarLoadBalancerAttributes
    , mlbarLoadBalancerName
    , mlbarStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyLoadBalancerAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlbaLoadBalancerName'
--
-- * 'mlbaLoadBalancerAttributes'
data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes'
    { _mlbaLoadBalancerName       :: !Text
    , _mlbaLoadBalancerAttributes :: !LoadBalancerAttributes
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyLoadBalancerAttributes' smart constructor.
modifyLoadBalancerAttributes :: Text -> LoadBalancerAttributes -> ModifyLoadBalancerAttributes
modifyLoadBalancerAttributes pLoadBalancerName pLoadBalancerAttributes =
    ModifyLoadBalancerAttributes'
    { _mlbaLoadBalancerName = pLoadBalancerName
    , _mlbaLoadBalancerAttributes = pLoadBalancerAttributes
    }

-- | The name of the load balancer.
mlbaLoadBalancerName :: Lens' ModifyLoadBalancerAttributes Text
mlbaLoadBalancerName = lens _mlbaLoadBalancerName (\ s a -> s{_mlbaLoadBalancerName = a});

-- | The attributes of the load balancer.
mlbaLoadBalancerAttributes :: Lens' ModifyLoadBalancerAttributes LoadBalancerAttributes
mlbaLoadBalancerAttributes = lens _mlbaLoadBalancerAttributes (\ s a -> s{_mlbaLoadBalancerAttributes = a});

instance AWSRequest ModifyLoadBalancerAttributes
         where
        type Sv ModifyLoadBalancerAttributes = ELB
        type Rs ModifyLoadBalancerAttributes =
             ModifyLoadBalancerAttributesResponse
        request = post
        response
          = receiveXMLWrapper
              "ModifyLoadBalancerAttributesResult"
              (\ s h x ->
                 ModifyLoadBalancerAttributesResponse' <$>
                   (x .@? "LoadBalancerAttributes") <*>
                     (x .@? "LoadBalancerName")
                     <*> (pure (fromEnum s)))

instance ToHeaders ModifyLoadBalancerAttributes where
        toHeaders = const mempty

instance ToPath ModifyLoadBalancerAttributes where
        toPath = const "/"

instance ToQuery ModifyLoadBalancerAttributes where
        toQuery ModifyLoadBalancerAttributes'{..}
          = mconcat
              ["Action" =:
                 ("ModifyLoadBalancerAttributes" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _mlbaLoadBalancerName,
               "LoadBalancerAttributes" =:
                 _mlbaLoadBalancerAttributes]

-- | /See:/ 'modifyLoadBalancerAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlbarLoadBalancerAttributes'
--
-- * 'mlbarLoadBalancerName'
--
-- * 'mlbarStatus'
data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse'
    { _mlbarLoadBalancerAttributes :: !(Maybe LoadBalancerAttributes)
    , _mlbarLoadBalancerName       :: !(Maybe Text)
    , _mlbarStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyLoadBalancerAttributesResponse' smart constructor.
modifyLoadBalancerAttributesResponse :: Int -> ModifyLoadBalancerAttributesResponse
modifyLoadBalancerAttributesResponse pStatus =
    ModifyLoadBalancerAttributesResponse'
    { _mlbarLoadBalancerAttributes = Nothing
    , _mlbarLoadBalancerName = Nothing
    , _mlbarStatus = pStatus
    }

-- | FIXME: Undocumented member.
mlbarLoadBalancerAttributes :: Lens' ModifyLoadBalancerAttributesResponse (Maybe LoadBalancerAttributes)
mlbarLoadBalancerAttributes = lens _mlbarLoadBalancerAttributes (\ s a -> s{_mlbarLoadBalancerAttributes = a});

-- | The name of the load balancer.
mlbarLoadBalancerName :: Lens' ModifyLoadBalancerAttributesResponse (Maybe Text)
mlbarLoadBalancerName = lens _mlbarLoadBalancerName (\ s a -> s{_mlbarLoadBalancerName = a});

-- | FIXME: Undocumented member.
mlbarStatus :: Lens' ModifyLoadBalancerAttributesResponse Int
mlbarStatus = lens _mlbarStatus (\ s a -> s{_mlbarStatus = a});
