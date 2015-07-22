{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateReusableDelegationSet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action creates a reusable delegationSet.
--
-- To create a new reusable delegationSet, send a @POST@ request to the
-- @2013-04-01\/delegationset@ resource. The request body must include an
-- XML document with a @CreateReusableDelegationSetRequest@ element. The
-- response returns the @CreateReusableDelegationSetResponse@ element that
-- contains metadata about the delegationSet.
--
-- If the optional parameter HostedZoneId is specified, it marks the
-- delegationSet associated with that particular hosted zone as reusable.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html>
module Network.AWS.Route53.CreateReusableDelegationSet
    (
    -- * Request
      CreateReusableDelegationSet
    -- ** Request constructor
    , createReusableDelegationSet
    -- ** Request lenses
    , crdsrqHostedZoneId
    , crdsrqCallerReference

    -- * Response
    , CreateReusableDelegationSetResponse
    -- ** Response constructor
    , createReusableDelegationSetResponse
    -- ** Response lenses
    , crdsrsStatus
    , crdsrsDelegationSet
    , crdsrsLocation
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | /See:/ 'createReusableDelegationSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crdsrqHostedZoneId'
--
-- * 'crdsrqCallerReference'
data CreateReusableDelegationSet = CreateReusableDelegationSet'
    { _crdsrqHostedZoneId    :: !(Maybe Text)
    , _crdsrqCallerReference :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateReusableDelegationSet' smart constructor.
createReusableDelegationSet :: Text -> CreateReusableDelegationSet
createReusableDelegationSet pCallerReference_ =
    CreateReusableDelegationSet'
    { _crdsrqHostedZoneId = Nothing
    , _crdsrqCallerReference = pCallerReference_
    }

-- | The ID of the hosted zone whose delegation set you want to mark as
-- reusable. It is an optional parameter.
crdsrqHostedZoneId :: Lens' CreateReusableDelegationSet (Maybe Text)
crdsrqHostedZoneId = lens _crdsrqHostedZoneId (\ s a -> s{_crdsrqHostedZoneId = a});

-- | A unique string that identifies the request and that allows failed
-- @CreateReusableDelegationSet@ requests to be retried without the risk of
-- executing the operation twice. You must use a unique @CallerReference@
-- string every time you create a reusable delegation set.
-- @CallerReference@ can be any unique string; you might choose to use a
-- string that identifies your project, such as @DNSMigration_01@.
--
-- Valid characters are any Unicode code points that are legal in an XML
-- 1.0 document. The UTF-8 encoding of the value must be less than 128
-- bytes.
crdsrqCallerReference :: Lens' CreateReusableDelegationSet Text
crdsrqCallerReference = lens _crdsrqCallerReference (\ s a -> s{_crdsrqCallerReference = a});

instance AWSRequest CreateReusableDelegationSet where
        type Sv CreateReusableDelegationSet = Route53
        type Rs CreateReusableDelegationSet =
             CreateReusableDelegationSetResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateReusableDelegationSetResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "DelegationSet") <*>
                     (h .# "Location"))

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
              ["HostedZoneId" @= _crdsrqHostedZoneId,
               "CallerReference" @= _crdsrqCallerReference]

-- | /See:/ 'createReusableDelegationSetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crdsrsStatus'
--
-- * 'crdsrsDelegationSet'
--
-- * 'crdsrsLocation'
data CreateReusableDelegationSetResponse = CreateReusableDelegationSetResponse'
    { _crdsrsStatus        :: !Int
    , _crdsrsDelegationSet :: !DelegationSet
    , _crdsrsLocation      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateReusableDelegationSetResponse' smart constructor.
createReusableDelegationSetResponse :: Int -> DelegationSet -> Text -> CreateReusableDelegationSetResponse
createReusableDelegationSetResponse pStatus_ pDelegationSet_ pLocation_ =
    CreateReusableDelegationSetResponse'
    { _crdsrsStatus = pStatus_
    , _crdsrsDelegationSet = pDelegationSet_
    , _crdsrsLocation = pLocation_
    }

-- | FIXME: Undocumented member.
crdsrsStatus :: Lens' CreateReusableDelegationSetResponse Int
crdsrsStatus = lens _crdsrsStatus (\ s a -> s{_crdsrsStatus = a});

-- | A complex type that contains name server information.
crdsrsDelegationSet :: Lens' CreateReusableDelegationSetResponse DelegationSet
crdsrsDelegationSet = lens _crdsrsDelegationSet (\ s a -> s{_crdsrsDelegationSet = a});

-- | The unique URL representing the new reusbale delegation set.
crdsrsLocation :: Lens' CreateReusableDelegationSetResponse Text
crdsrsLocation = lens _crdsrsLocation (\ s a -> s{_crdsrsLocation = a});
