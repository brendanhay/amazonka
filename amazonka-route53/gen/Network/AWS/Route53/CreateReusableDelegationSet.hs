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
-- Module      : Network.AWS.Route53.CreateReusableDelegationSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a delegation set (a group of four name servers) that can be reused by multiple hosted zones. If a hosted zoned ID is specified, @CreateReusableDelegationSet@ marks the delegation set associated with that zone as reusable.
--
--
-- For information about using a reusable delegation set to configure white label name servers, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/white-label-name-servers.html Configuring White Label Name Servers> .
--
-- The process for migrating existing hosted zones to use a reusable delegation set is comparable to the process for configuring white label name servers. You need to perform the following steps:
--
--     * Create a reusable delegation set.
--
--     * Recreate hosted zones, and reduce the TTL to 60 seconds or less.
--
--     * Recreate resource record sets in the new hosted zones.
--
--     * Change the registrar's name servers to use the name servers for the new hosted zones.
--
--     * Monitor traffic for the website or application.
--
--     * Change TTLs back to their original values.
--
--
--
-- If you want to migrate existing hosted zones to use a reusable delegation set, the existing hosted zones can't use any of the name servers that are assigned to the reusable delegation set. If one or more hosted zones do use one or more name servers that are assigned to the reusable delegation set, you can do one of the following:
--
--     * For small numbers of hosted zones—up to a few hundred—it's relatively easy to create reusable delegation sets until you get one that has four name servers that don't overlap with any of the name servers in your hosted zones.
--
--     * For larger numbers of hosted zones, the easiest solution is to use more than one reusable delegation set.
--
--     * For larger numbers of hosted zones, you can also migrate hosted zones that have overlapping name servers to hosted zones that don't have overlapping name servers, then migrate the hosted zones again to use the reusable delegation set.
--
--
--
module Network.AWS.Route53.CreateReusableDelegationSet
    (
    -- * Creating a Request
      createReusableDelegationSet
    , CreateReusableDelegationSet
    -- * Request Lenses
    , crdsHostedZoneId
    , crdsCallerReference

    -- * Destructuring the Response
    , createReusableDelegationSetResponse
    , CreateReusableDelegationSetResponse
    -- * Response Lenses
    , crdsrsResponseStatus
    , crdsrsDelegationSet
    , crdsrsLocation
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | /See:/ 'createReusableDelegationSet' smart constructor.
data CreateReusableDelegationSet = CreateReusableDelegationSet'
  { _crdsHostedZoneId    :: !(Maybe ResourceId)
  , _crdsCallerReference :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReusableDelegationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdsHostedZoneId' - If you want to mark the delegation set for an existing hosted zone as reusable, the ID for that hosted zone.
--
-- * 'crdsCallerReference' - A unique string that identifies the request, and that allows you to retry failed @CreateReusableDelegationSet@ requests without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateReusableDelegationSet@ request. @CallerReference@ can be any unique string, for example a date/time stamp.
createReusableDelegationSet
    :: Text -- ^ 'crdsCallerReference'
    -> CreateReusableDelegationSet
createReusableDelegationSet pCallerReference_ =
  CreateReusableDelegationSet'
    {_crdsHostedZoneId = Nothing, _crdsCallerReference = pCallerReference_}


-- | If you want to mark the delegation set for an existing hosted zone as reusable, the ID for that hosted zone.
crdsHostedZoneId :: Lens' CreateReusableDelegationSet (Maybe ResourceId)
crdsHostedZoneId = lens _crdsHostedZoneId (\ s a -> s{_crdsHostedZoneId = a})

-- | A unique string that identifies the request, and that allows you to retry failed @CreateReusableDelegationSet@ requests without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateReusableDelegationSet@ request. @CallerReference@ can be any unique string, for example a date/time stamp.
crdsCallerReference :: Lens' CreateReusableDelegationSet Text
crdsCallerReference = lens _crdsCallerReference (\ s a -> s{_crdsCallerReference = a})

instance AWSRequest CreateReusableDelegationSet where
        type Rs CreateReusableDelegationSet =
             CreateReusableDelegationSetResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 CreateReusableDelegationSetResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "DelegationSet") <*>
                     (h .# "Location"))

instance Hashable CreateReusableDelegationSet where

instance NFData CreateReusableDelegationSet where

instance ToElement CreateReusableDelegationSet where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateReusableDelegationSetRequest"

instance ToHeaders CreateReusableDelegationSet where
        toHeaders = const mempty

instance ToPath CreateReusableDelegationSet where
        toPath = const "/2013-04-01/delegationset"

instance ToQuery CreateReusableDelegationSet where
        toQuery = const mempty

instance ToXML CreateReusableDelegationSet where
        toXML CreateReusableDelegationSet'{..}
          = mconcat
              ["HostedZoneId" @= _crdsHostedZoneId,
               "CallerReference" @= _crdsCallerReference]

-- | /See:/ 'createReusableDelegationSetResponse' smart constructor.
data CreateReusableDelegationSetResponse = CreateReusableDelegationSetResponse'
  { _crdsrsResponseStatus :: !Int
  , _crdsrsDelegationSet  :: !DelegationSet
  , _crdsrsLocation       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReusableDelegationSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdsrsResponseStatus' - -- | The response status code.
--
-- * 'crdsrsDelegationSet' - A complex type that contains name server information.
--
-- * 'crdsrsLocation' - The unique URL representing the new reusable delegation set.
createReusableDelegationSetResponse
    :: Int -- ^ 'crdsrsResponseStatus'
    -> DelegationSet -- ^ 'crdsrsDelegationSet'
    -> Text -- ^ 'crdsrsLocation'
    -> CreateReusableDelegationSetResponse
createReusableDelegationSetResponse pResponseStatus_ pDelegationSet_ pLocation_ =
  CreateReusableDelegationSetResponse'
    { _crdsrsResponseStatus = pResponseStatus_
    , _crdsrsDelegationSet = pDelegationSet_
    , _crdsrsLocation = pLocation_
    }


-- | -- | The response status code.
crdsrsResponseStatus :: Lens' CreateReusableDelegationSetResponse Int
crdsrsResponseStatus = lens _crdsrsResponseStatus (\ s a -> s{_crdsrsResponseStatus = a})

-- | A complex type that contains name server information.
crdsrsDelegationSet :: Lens' CreateReusableDelegationSetResponse DelegationSet
crdsrsDelegationSet = lens _crdsrsDelegationSet (\ s a -> s{_crdsrsDelegationSet = a})

-- | The unique URL representing the new reusable delegation set.
crdsrsLocation :: Lens' CreateReusableDelegationSetResponse Text
crdsrsLocation = lens _crdsrsLocation (\ s a -> s{_crdsrsLocation = a})

instance NFData CreateReusableDelegationSetResponse
         where
