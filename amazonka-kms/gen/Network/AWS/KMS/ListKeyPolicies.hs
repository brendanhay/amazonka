{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListKeyPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of policies attached to a key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_ListKeyPolicies.html>
module Network.AWS.KMS.ListKeyPolicies
    (
    -- * Request
      ListKeyPolicies
    -- ** Request constructor
    , listKeyPolicies
    -- ** Request lenses
    , lkpMarker
    , lkpLimit
    , lkpKeyId

    -- * Response
    , ListKeyPoliciesResponse
    -- ** Response constructor
    , listKeyPoliciesResponse
    -- ** Response lenses
    , lkprsPolicyNames
    , lkprsTruncated
    , lkprsNextMarker
    , lkprsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listKeyPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lkpMarker'
--
-- * 'lkpLimit'
--
-- * 'lkpKeyId'
data ListKeyPolicies = ListKeyPolicies'
    { _lkpMarker :: !(Maybe Text)
    , _lkpLimit  :: !(Maybe Nat)
    , _lkpKeyId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListKeyPolicies' smart constructor.
listKeyPolicies :: Text -> ListKeyPolicies
listKeyPolicies pKeyId_ =
    ListKeyPolicies'
    { _lkpMarker = Nothing
    , _lkpLimit = Nothing
    , _lkpKeyId = pKeyId_
    }

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @NextMarker@ in the response
-- you just received.
lkpMarker :: Lens' ListKeyPolicies (Maybe Text)
lkpMarker = lens _lkpMarker (\ s a -> s{_lkpMarker = a});

-- | Specify this parameter only when paginating results to indicate the
-- maximum number of policies you want listed in the response. If there are
-- additional policies beyond the maximum you specify, the @Truncated@
-- response element will be set to @true.@
lkpLimit :: Lens' ListKeyPolicies (Maybe Natural)
lkpLimit = lens _lkpLimit (\ s a -> s{_lkpLimit = a}) . mapping _Nat;

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier, a fully specified ARN to either an alias or
-- a key, or an alias name prefixed by \"alias\/\".
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Alias ARN Example -
--     arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
-- -   Alias Name Example - alias\/MyAliasName
lkpKeyId :: Lens' ListKeyPolicies Text
lkpKeyId = lens _lkpKeyId (\ s a -> s{_lkpKeyId = a});

instance AWSRequest ListKeyPolicies where
        type Sv ListKeyPolicies = KMS
        type Rs ListKeyPolicies = ListKeyPoliciesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListKeyPoliciesResponse' <$>
                   (x .?> "PolicyNames" .!@ mempty) <*>
                     (x .?> "Truncated")
                     <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListKeyPolicies where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ListKeyPolicies" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListKeyPolicies where
        toJSON ListKeyPolicies'{..}
          = object
              ["Marker" .= _lkpMarker, "Limit" .= _lkpLimit,
               "KeyId" .= _lkpKeyId]

instance ToPath ListKeyPolicies where
        toPath = const "/"

instance ToQuery ListKeyPolicies where
        toQuery = const mempty

-- | /See:/ 'listKeyPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lkprsPolicyNames'
--
-- * 'lkprsTruncated'
--
-- * 'lkprsNextMarker'
--
-- * 'lkprsStatus'
data ListKeyPoliciesResponse = ListKeyPoliciesResponse'
    { _lkprsPolicyNames :: !(Maybe [Text])
    , _lkprsTruncated   :: !(Maybe Bool)
    , _lkprsNextMarker  :: !(Maybe Text)
    , _lkprsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListKeyPoliciesResponse' smart constructor.
listKeyPoliciesResponse :: Int -> ListKeyPoliciesResponse
listKeyPoliciesResponse pStatus_ =
    ListKeyPoliciesResponse'
    { _lkprsPolicyNames = Nothing
    , _lkprsTruncated = Nothing
    , _lkprsNextMarker = Nothing
    , _lkprsStatus = pStatus_
    }

-- | A list of policy names. Currently, there is only one policy and it is
-- named \"Default\".
lkprsPolicyNames :: Lens' ListKeyPoliciesResponse [Text]
lkprsPolicyNames = lens _lkprsPolicyNames (\ s a -> s{_lkprsPolicyNames = a}) . _Default . _Coerce;

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more policies in the
-- list.
lkprsTruncated :: Lens' ListKeyPoliciesResponse (Maybe Bool)
lkprsTruncated = lens _lkprsTruncated (\ s a -> s{_lkprsTruncated = a});

-- | If @Truncated@ is true, this value is present and contains the value to
-- use for the @Marker@ request parameter in a subsequent pagination
-- request.
lkprsNextMarker :: Lens' ListKeyPoliciesResponse (Maybe Text)
lkprsNextMarker = lens _lkprsNextMarker (\ s a -> s{_lkprsNextMarker = a});

-- | FIXME: Undocumented member.
lkprsStatus :: Lens' ListKeyPoliciesResponse Int
lkprsStatus = lens _lkprsStatus (\ s a -> s{_lkprsStatus = a});
