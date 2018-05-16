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
-- Module      : Network.AWS.Route53.CreateTrafficPolicyInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates resource record sets in a specified hosted zone based on the settings in a specified traffic policy version. In addition, @CreateTrafficPolicyInstance@ associates the resource record sets with a specified domain name (such as example.com) or subdomain name (such as www.example.com). Amazon Route 53 responds to DNS queries for the domain or subdomain name by using the resource record sets that @CreateTrafficPolicyInstance@ created.
--
--
module Network.AWS.Route53.CreateTrafficPolicyInstance
    (
    -- * Creating a Request
      createTrafficPolicyInstance
    , CreateTrafficPolicyInstance
    -- * Request Lenses
    , ctpiHostedZoneId
    , ctpiName
    , ctpiTTL
    , ctpiTrafficPolicyId
    , ctpiTrafficPolicyVersion

    -- * Destructuring the Response
    , createTrafficPolicyInstanceResponse
    , CreateTrafficPolicyInstanceResponse
    -- * Response Lenses
    , ctpirsResponseStatus
    , ctpirsTrafficPolicyInstance
    , ctpirsLocation
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the resource record sets that you want to create based on a specified traffic policy.
--
--
--
-- /See:/ 'createTrafficPolicyInstance' smart constructor.
data CreateTrafficPolicyInstance = CreateTrafficPolicyInstance'
  { _ctpiHostedZoneId         :: !ResourceId
  , _ctpiName                 :: !Text
  , _ctpiTTL                  :: !Nat
  , _ctpiTrafficPolicyId      :: !Text
  , _ctpiTrafficPolicyVersion :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTrafficPolicyInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctpiHostedZoneId' - The ID of the hosted zone in which you want Amazon Route 53 to create resource record sets by using the configuration in a traffic policy.
--
-- * 'ctpiName' - The domain name (such as example.com) or subdomain name (such as www.example.com) for which Amazon Route 53 responds to DNS queries by using the resource record sets that Amazon Route 53 creates for this traffic policy instance.
--
-- * 'ctpiTTL' - (Optional) The TTL that you want Amazon Route 53 to assign to all of the resource record sets that it creates in the specified hosted zone.
--
-- * 'ctpiTrafficPolicyId' - The ID of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
--
-- * 'ctpiTrafficPolicyVersion' - The version of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
createTrafficPolicyInstance
    :: ResourceId -- ^ 'ctpiHostedZoneId'
    -> Text -- ^ 'ctpiName'
    -> Natural -- ^ 'ctpiTTL'
    -> Text -- ^ 'ctpiTrafficPolicyId'
    -> Natural -- ^ 'ctpiTrafficPolicyVersion'
    -> CreateTrafficPolicyInstance
createTrafficPolicyInstance pHostedZoneId_ pName_ pTTL_ pTrafficPolicyId_ pTrafficPolicyVersion_ =
  CreateTrafficPolicyInstance'
    { _ctpiHostedZoneId = pHostedZoneId_
    , _ctpiName = pName_
    , _ctpiTTL = _Nat # pTTL_
    , _ctpiTrafficPolicyId = pTrafficPolicyId_
    , _ctpiTrafficPolicyVersion = _Nat # pTrafficPolicyVersion_
    }


-- | The ID of the hosted zone in which you want Amazon Route 53 to create resource record sets by using the configuration in a traffic policy.
ctpiHostedZoneId :: Lens' CreateTrafficPolicyInstance ResourceId
ctpiHostedZoneId = lens _ctpiHostedZoneId (\ s a -> s{_ctpiHostedZoneId = a})

-- | The domain name (such as example.com) or subdomain name (such as www.example.com) for which Amazon Route 53 responds to DNS queries by using the resource record sets that Amazon Route 53 creates for this traffic policy instance.
ctpiName :: Lens' CreateTrafficPolicyInstance Text
ctpiName = lens _ctpiName (\ s a -> s{_ctpiName = a})

-- | (Optional) The TTL that you want Amazon Route 53 to assign to all of the resource record sets that it creates in the specified hosted zone.
ctpiTTL :: Lens' CreateTrafficPolicyInstance Natural
ctpiTTL = lens _ctpiTTL (\ s a -> s{_ctpiTTL = a}) . _Nat

-- | The ID of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
ctpiTrafficPolicyId :: Lens' CreateTrafficPolicyInstance Text
ctpiTrafficPolicyId = lens _ctpiTrafficPolicyId (\ s a -> s{_ctpiTrafficPolicyId = a})

-- | The version of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
ctpiTrafficPolicyVersion :: Lens' CreateTrafficPolicyInstance Natural
ctpiTrafficPolicyVersion = lens _ctpiTrafficPolicyVersion (\ s a -> s{_ctpiTrafficPolicyVersion = a}) . _Nat

instance AWSRequest CreateTrafficPolicyInstance where
        type Rs CreateTrafficPolicyInstance =
             CreateTrafficPolicyInstanceResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 CreateTrafficPolicyInstanceResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@ "TrafficPolicyInstance")
                     <*> (h .# "Location"))

instance Hashable CreateTrafficPolicyInstance where

instance NFData CreateTrafficPolicyInstance where

instance ToElement CreateTrafficPolicyInstance where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyInstanceRequest"

instance ToHeaders CreateTrafficPolicyInstance where
        toHeaders = const mempty

instance ToPath CreateTrafficPolicyInstance where
        toPath = const "/2013-04-01/trafficpolicyinstance"

instance ToQuery CreateTrafficPolicyInstance where
        toQuery = const mempty

instance ToXML CreateTrafficPolicyInstance where
        toXML CreateTrafficPolicyInstance'{..}
          = mconcat
              ["HostedZoneId" @= _ctpiHostedZoneId,
               "Name" @= _ctpiName, "TTL" @= _ctpiTTL,
               "TrafficPolicyId" @= _ctpiTrafficPolicyId,
               "TrafficPolicyVersion" @= _ctpiTrafficPolicyVersion]

-- | A complex type that contains the response information for the @CreateTrafficPolicyInstance@ request.
--
--
--
-- /See:/ 'createTrafficPolicyInstanceResponse' smart constructor.
data CreateTrafficPolicyInstanceResponse = CreateTrafficPolicyInstanceResponse'
  { _ctpirsResponseStatus        :: !Int
  , _ctpirsTrafficPolicyInstance :: !TrafficPolicyInstance
  , _ctpirsLocation              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTrafficPolicyInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctpirsResponseStatus' - -- | The response status code.
--
-- * 'ctpirsTrafficPolicyInstance' - A complex type that contains settings for the new traffic policy instance.
--
-- * 'ctpirsLocation' - A unique URL that represents a new traffic policy instance.
createTrafficPolicyInstanceResponse
    :: Int -- ^ 'ctpirsResponseStatus'
    -> TrafficPolicyInstance -- ^ 'ctpirsTrafficPolicyInstance'
    -> Text -- ^ 'ctpirsLocation'
    -> CreateTrafficPolicyInstanceResponse
createTrafficPolicyInstanceResponse pResponseStatus_ pTrafficPolicyInstance_ pLocation_ =
  CreateTrafficPolicyInstanceResponse'
    { _ctpirsResponseStatus = pResponseStatus_
    , _ctpirsTrafficPolicyInstance = pTrafficPolicyInstance_
    , _ctpirsLocation = pLocation_
    }


-- | -- | The response status code.
ctpirsResponseStatus :: Lens' CreateTrafficPolicyInstanceResponse Int
ctpirsResponseStatus = lens _ctpirsResponseStatus (\ s a -> s{_ctpirsResponseStatus = a})

-- | A complex type that contains settings for the new traffic policy instance.
ctpirsTrafficPolicyInstance :: Lens' CreateTrafficPolicyInstanceResponse TrafficPolicyInstance
ctpirsTrafficPolicyInstance = lens _ctpirsTrafficPolicyInstance (\ s a -> s{_ctpirsTrafficPolicyInstance = a})

-- | A unique URL that represents a new traffic policy instance.
ctpirsLocation :: Lens' CreateTrafficPolicyInstanceResponse Text
ctpirsLocation = lens _ctpirsLocation (\ s a -> s{_ctpirsLocation = a})

instance NFData CreateTrafficPolicyInstanceResponse
         where
