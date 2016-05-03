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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action creates a reusable delegationSet.
--
-- To create a new reusable delegationSet, send a 'POST' request to the
-- '\/Route 53 API version\/delegationset' resource. The request body must
-- include a document with a 'CreateReusableDelegationSetRequest' element.
-- The response returns the 'CreateReusableDelegationSetResponse' element
-- that contains metadata about the delegationSet.
--
-- If the optional parameter HostedZoneId is specified, it marks the
-- delegationSet associated with that particular hosted zone as reusable.
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | /See:/ 'createReusableDelegationSet' smart constructor.
data CreateReusableDelegationSet = CreateReusableDelegationSet'
    { _crdsHostedZoneId    :: !(Maybe Text)
    , _crdsCallerReference :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateReusableDelegationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdsHostedZoneId'
--
-- * 'crdsCallerReference'
createReusableDelegationSet
    :: Text -- ^ 'crdsCallerReference'
    -> CreateReusableDelegationSet
createReusableDelegationSet pCallerReference_ =
    CreateReusableDelegationSet'
    { _crdsHostedZoneId = Nothing
    , _crdsCallerReference = pCallerReference_
    }

-- | The ID of the hosted zone whose delegation set you want to mark as
-- reusable. It is an optional parameter.
crdsHostedZoneId :: Lens' CreateReusableDelegationSet (Maybe Text)
crdsHostedZoneId = lens _crdsHostedZoneId (\ s a -> s{_crdsHostedZoneId = a});

-- | A unique string that identifies the request and that allows failed
-- 'CreateReusableDelegationSet' requests to be retried without the risk of
-- executing the operation twice. You must use a unique 'CallerReference'
-- string every time you create a reusable delegation set.
-- 'CallerReference' can be any unique string; you might choose to use a
-- string that identifies your project, such as 'DNSMigration_01'.
--
-- Valid characters are any Unicode code points that are legal in an XML
-- 1.0 document. The UTF-8 encoding of the value must be less than 128
-- bytes.
crdsCallerReference :: Lens' CreateReusableDelegationSet Text
crdsCallerReference = lens _crdsCallerReference (\ s a -> s{_crdsCallerReference = a});

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

instance Hashable CreateReusableDelegationSet

instance NFData CreateReusableDelegationSet

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateReusableDelegationSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdsrsResponseStatus'
--
-- * 'crdsrsDelegationSet'
--
-- * 'crdsrsLocation'
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

-- | The response status code.
crdsrsResponseStatus :: Lens' CreateReusableDelegationSetResponse Int
crdsrsResponseStatus = lens _crdsrsResponseStatus (\ s a -> s{_crdsrsResponseStatus = a});

-- | A complex type that contains name server information.
crdsrsDelegationSet :: Lens' CreateReusableDelegationSetResponse DelegationSet
crdsrsDelegationSet = lens _crdsrsDelegationSet (\ s a -> s{_crdsrsDelegationSet = a});

-- | The unique URL representing the new reusbale delegation set.
crdsrsLocation :: Lens' CreateReusableDelegationSetResponse Text
crdsrsLocation = lens _crdsrsLocation (\ s a -> s{_crdsrsLocation = a});

instance NFData CreateReusableDelegationSetResponse
