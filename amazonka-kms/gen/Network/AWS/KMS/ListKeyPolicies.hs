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
-- Module      : Network.AWS.KMS.ListKeyPolicies
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of policies attached to a key.
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListKeyPolicies
    (
    -- * Creating a Request
      listKeyPolicies
    , ListKeyPolicies
    -- * Request Lenses
    , lkpMarker
    , lkpLimit
    , lkpKeyId

    -- * Destructuring the Response
    , listKeyPoliciesResponse
    , ListKeyPoliciesResponse
    -- * Response Lenses
    , lkprsPolicyNames
    , lkprsTruncated
    , lkprsNextMarker
    , lkprsResponseStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listKeyPolicies' smart constructor.
data ListKeyPolicies = ListKeyPolicies'
    { _lkpMarker :: !(Maybe Text)
    , _lkpLimit  :: !(Maybe Nat)
    , _lkpKeyId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListKeyPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lkpMarker'
--
-- * 'lkpLimit'
--
-- * 'lkpKeyId'
listKeyPolicies
    :: Text -- ^ 'lkpKeyId'
    -> ListKeyPolicies
listKeyPolicies pKeyId_ =
    ListKeyPolicies'
    { _lkpMarker = Nothing
    , _lkpLimit = Nothing
    , _lkpKeyId = pKeyId_
    }

-- | Use this parameter only when paginating results and only in a subsequent request after you receive a response with truncated results. Set it to the value of 'NextMarker' from the response you just received.
lkpMarker :: Lens' ListKeyPolicies (Maybe Text)
lkpMarker = lens _lkpMarker (\ s a -> s{_lkpMarker = a});

-- | When paginating results, specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the 'Truncated' element in the response is set to true.
--
-- This value is optional. If you include a value, it must be between 1 and 1000, inclusive. If you do not include a value, it defaults to 100.
--
-- Currently only 1 policy can be attached to a key.
lkpLimit :: Lens' ListKeyPolicies (Maybe Natural)
lkpLimit = lens _lkpLimit (\ s a -> s{_lkpLimit = a}) . mapping _Nat;

-- | A unique identifier for the customer master key. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by \"alias\/\".
--
-- -   Key ARN Example - arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Alias ARN Example - arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName
-- -   Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
-- -   Alias Name Example - alias\/MyAliasName
lkpKeyId :: Lens' ListKeyPolicies Text
lkpKeyId = lens _lkpKeyId (\ s a -> s{_lkpKeyId = a});

instance AWSPager ListKeyPolicies where
        page rq rs
          | stop (rs ^. lkprsTruncated) = Nothing
          | isNothing (rs ^. lkprsNextMarker) = Nothing
          | otherwise =
            Just $ rq & lkpMarker .~ rs ^. lkprsNextMarker

instance AWSRequest ListKeyPolicies where
        type Rs ListKeyPolicies = ListKeyPoliciesResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 ListKeyPoliciesResponse' <$>
                   (x .?> "PolicyNames" .!@ mempty) <*>
                     (x .?> "Truncated")
                     <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListKeyPolicies

instance NFData ListKeyPolicies

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
              (catMaybes
                 [("Marker" .=) <$> _lkpMarker,
                  ("Limit" .=) <$> _lkpLimit,
                  Just ("KeyId" .= _lkpKeyId)])

instance ToPath ListKeyPolicies where
        toPath = const "/"

instance ToQuery ListKeyPolicies where
        toQuery = const mempty

-- | /See:/ 'listKeyPoliciesResponse' smart constructor.
data ListKeyPoliciesResponse = ListKeyPoliciesResponse'
    { _lkprsPolicyNames    :: !(Maybe [Text])
    , _lkprsTruncated      :: !(Maybe Bool)
    , _lkprsNextMarker     :: !(Maybe Text)
    , _lkprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListKeyPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lkprsPolicyNames'
--
-- * 'lkprsTruncated'
--
-- * 'lkprsNextMarker'
--
-- * 'lkprsResponseStatus'
listKeyPoliciesResponse
    :: Int -- ^ 'lkprsResponseStatus'
    -> ListKeyPoliciesResponse
listKeyPoliciesResponse pResponseStatus_ =
    ListKeyPoliciesResponse'
    { _lkprsPolicyNames = Nothing
    , _lkprsTruncated = Nothing
    , _lkprsNextMarker = Nothing
    , _lkprsResponseStatus = pResponseStatus_
    }

-- | A list of policy names. Currently, there is only one policy and it is named \"Default\".
lkprsPolicyNames :: Lens' ListKeyPoliciesResponse [Text]
lkprsPolicyNames = lens _lkprsPolicyNames (\ s a -> s{_lkprsPolicyNames = a}) . _Default . _Coerce;

-- | A flag that indicates whether there are more items in the list. If your results were truncated, you can use the 'Marker' parameter to make a subsequent pagination request to retrieve more items in the list.
lkprsTruncated :: Lens' ListKeyPoliciesResponse (Maybe Bool)
lkprsTruncated = lens _lkprsTruncated (\ s a -> s{_lkprsTruncated = a});

-- | When 'Truncated' is true, this value is present and contains the value to use for the 'Marker' parameter in a subsequent pagination request.
lkprsNextMarker :: Lens' ListKeyPoliciesResponse (Maybe Text)
lkprsNextMarker = lens _lkprsNextMarker (\ s a -> s{_lkprsNextMarker = a});

-- | The response status code.
lkprsResponseStatus :: Lens' ListKeyPoliciesResponse Int
lkprsResponseStatus = lens _lkprsResponseStatus (\ s a -> s{_lkprsResponseStatus = a});

instance NFData ListKeyPoliciesResponse
