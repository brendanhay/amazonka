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
-- Module      : Network.AWS.Route53.UpdateTrafficPolicyInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource record sets in a specified hosted zone that were created based on the settings in a specified traffic policy version.
--
--
-- When you update a traffic policy instance, Amazon Route 53 continues to respond to DNS queries for the root resource record set name (such as example.com) while it replaces one group of resource record sets with another. Amazon Route 53 performs the following operations:
--
--     * Amazon Route 53 creates a new group of resource record sets based on the specified traffic policy. This is true regardless of how significant the differences are between the existing resource record sets and the new resource record sets.
--
--     * When all of the new resource record sets have been created, Amazon Route 53 starts to respond to DNS queries for the root resource record set name (such as example.com) by using the new resource record sets.
--
--     * Amazon Route 53 deletes the old group of resource record sets that are associated with the root resource record set name.
--
--
--
module Network.AWS.Route53.UpdateTrafficPolicyInstance
    (
    -- * Creating a Request
      updateTrafficPolicyInstance
    , UpdateTrafficPolicyInstance
    -- * Request Lenses
    , utpiId
    , utpiTTL
    , utpiTrafficPolicyId
    , utpiTrafficPolicyVersion

    -- * Destructuring the Response
    , updateTrafficPolicyInstanceResponse
    , UpdateTrafficPolicyInstanceResponse
    -- * Response Lenses
    , utpirsResponseStatus
    , utpirsTrafficPolicyInstance
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the resource record sets that you want to update based on a specified traffic policy instance.
--
--
--
-- /See:/ 'updateTrafficPolicyInstance' smart constructor.
data UpdateTrafficPolicyInstance = UpdateTrafficPolicyInstance'
  { _utpiId                   :: !Text
  , _utpiTTL                  :: !Nat
  , _utpiTrafficPolicyId      :: !Text
  , _utpiTrafficPolicyVersion :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTrafficPolicyInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utpiId' - The ID of the traffic policy instance that you want to update.
--
-- * 'utpiTTL' - The TTL that you want Amazon Route 53 to assign to all of the updated resource record sets.
--
-- * 'utpiTrafficPolicyId' - The ID of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
--
-- * 'utpiTrafficPolicyVersion' - The version of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
updateTrafficPolicyInstance
    :: Text -- ^ 'utpiId'
    -> Natural -- ^ 'utpiTTL'
    -> Text -- ^ 'utpiTrafficPolicyId'
    -> Natural -- ^ 'utpiTrafficPolicyVersion'
    -> UpdateTrafficPolicyInstance
updateTrafficPolicyInstance pId_ pTTL_ pTrafficPolicyId_ pTrafficPolicyVersion_ =
  UpdateTrafficPolicyInstance'
    { _utpiId = pId_
    , _utpiTTL = _Nat # pTTL_
    , _utpiTrafficPolicyId = pTrafficPolicyId_
    , _utpiTrafficPolicyVersion = _Nat # pTrafficPolicyVersion_
    }


-- | The ID of the traffic policy instance that you want to update.
utpiId :: Lens' UpdateTrafficPolicyInstance Text
utpiId = lens _utpiId (\ s a -> s{_utpiId = a})

-- | The TTL that you want Amazon Route 53 to assign to all of the updated resource record sets.
utpiTTL :: Lens' UpdateTrafficPolicyInstance Natural
utpiTTL = lens _utpiTTL (\ s a -> s{_utpiTTL = a}) . _Nat

-- | The ID of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
utpiTrafficPolicyId :: Lens' UpdateTrafficPolicyInstance Text
utpiTrafficPolicyId = lens _utpiTrafficPolicyId (\ s a -> s{_utpiTrafficPolicyId = a})

-- | The version of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
utpiTrafficPolicyVersion :: Lens' UpdateTrafficPolicyInstance Natural
utpiTrafficPolicyVersion = lens _utpiTrafficPolicyVersion (\ s a -> s{_utpiTrafficPolicyVersion = a}) . _Nat

instance AWSRequest UpdateTrafficPolicyInstance where
        type Rs UpdateTrafficPolicyInstance =
             UpdateTrafficPolicyInstanceResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 UpdateTrafficPolicyInstanceResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@ "TrafficPolicyInstance"))

instance Hashable UpdateTrafficPolicyInstance where

instance NFData UpdateTrafficPolicyInstance where

instance ToElement UpdateTrafficPolicyInstance where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateTrafficPolicyInstanceRequest"

instance ToHeaders UpdateTrafficPolicyInstance where
        toHeaders = const mempty

instance ToPath UpdateTrafficPolicyInstance where
        toPath UpdateTrafficPolicyInstance'{..}
          = mconcat
              ["/2013-04-01/trafficpolicyinstance/", toBS _utpiId]

instance ToQuery UpdateTrafficPolicyInstance where
        toQuery = const mempty

instance ToXML UpdateTrafficPolicyInstance where
        toXML UpdateTrafficPolicyInstance'{..}
          = mconcat
              ["TTL" @= _utpiTTL,
               "TrafficPolicyId" @= _utpiTrafficPolicyId,
               "TrafficPolicyVersion" @= _utpiTrafficPolicyVersion]

-- | A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.
--
--
--
-- /See:/ 'updateTrafficPolicyInstanceResponse' smart constructor.
data UpdateTrafficPolicyInstanceResponse = UpdateTrafficPolicyInstanceResponse'
  { _utpirsResponseStatus        :: !Int
  , _utpirsTrafficPolicyInstance :: !TrafficPolicyInstance
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTrafficPolicyInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utpirsResponseStatus' - -- | The response status code.
--
-- * 'utpirsTrafficPolicyInstance' - A complex type that contains settings for the updated traffic policy instance.
updateTrafficPolicyInstanceResponse
    :: Int -- ^ 'utpirsResponseStatus'
    -> TrafficPolicyInstance -- ^ 'utpirsTrafficPolicyInstance'
    -> UpdateTrafficPolicyInstanceResponse
updateTrafficPolicyInstanceResponse pResponseStatus_ pTrafficPolicyInstance_ =
  UpdateTrafficPolicyInstanceResponse'
    { _utpirsResponseStatus = pResponseStatus_
    , _utpirsTrafficPolicyInstance = pTrafficPolicyInstance_
    }


-- | -- | The response status code.
utpirsResponseStatus :: Lens' UpdateTrafficPolicyInstanceResponse Int
utpirsResponseStatus = lens _utpirsResponseStatus (\ s a -> s{_utpirsResponseStatus = a})

-- | A complex type that contains settings for the updated traffic policy instance.
utpirsTrafficPolicyInstance :: Lens' UpdateTrafficPolicyInstanceResponse TrafficPolicyInstance
utpirsTrafficPolicyInstance = lens _utpirsTrafficPolicyInstance (\ s a -> s{_utpirsTrafficPolicyInstance = a})

instance NFData UpdateTrafficPolicyInstanceResponse
         where
