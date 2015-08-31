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
-- Module      : Network.AWS.KMS.ListGrants
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the grants for a specified key.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_ListGrants.html AWS API Reference> for ListGrants.
module Network.AWS.KMS.ListGrants
    (
    -- * Creating a Request
      listGrants
    , ListGrants
    -- * Request Lenses
    , lgMarker
    , lgLimit
    , lgKeyId

    -- * Destructuring the Response
    , listGrantsResponse
    , ListGrantsResponse
    -- * Response Lenses
    , lgrsTruncated
    , lgrsGrants
    , lgrsNextMarker
    , lgrsResponseStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listGrants' smart constructor.
data ListGrants = ListGrants'
    { _lgMarker :: !(Maybe Text)
    , _lgLimit  :: !(Maybe Nat)
    , _lgKeyId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGrants' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgMarker'
--
-- * 'lgLimit'
--
-- * 'lgKeyId'
listGrants
    :: Text -- ^ 'lgKeyId'
    -> ListGrants
listGrants pKeyId_ =
    ListGrants'
    { _lgMarker = Nothing
    , _lgLimit = Nothing
    , _lgKeyId = pKeyId_
    }

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the 'NextMarker' in the response
-- you just received.
lgMarker :: Lens' ListGrants (Maybe Text)
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a});

-- | Specify this parameter only when paginating results to indicate the
-- maximum number of grants you want listed in the response. If there are
-- additional grants beyond the maximum you specify, the 'Truncated'
-- response element will be set to 'true.'
lgLimit :: Lens' ListGrants (Maybe Natural)
lgLimit = lens _lgLimit (\ s a -> s{_lgLimit = a}) . mapping _Nat;

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
lgKeyId :: Lens' ListGrants Text
lgKeyId = lens _lgKeyId (\ s a -> s{_lgKeyId = a});

instance AWSRequest ListGrants where
        type Rs ListGrants = ListGrantsResponse
        request = postJSON kMS
        response
          = receiveJSON
              (\ s h x ->
                 ListGrantsResponse' <$>
                   (x .?> "Truncated") <*> (x .?> "Grants" .!@ mempty)
                     <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListGrants where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ListGrants" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListGrants where
        toJSON ListGrants'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _lgMarker,
                  ("Limit" .=) <$> _lgLimit,
                  Just ("KeyId" .= _lgKeyId)])

instance ToPath ListGrants where
        toPath = const "/"

instance ToQuery ListGrants where
        toQuery = const mempty

-- | /See:/ 'listGrantsResponse' smart constructor.
data ListGrantsResponse = ListGrantsResponse'
    { _lgrsTruncated      :: !(Maybe Bool)
    , _lgrsGrants         :: !(Maybe [GrantListEntry])
    , _lgrsNextMarker     :: !(Maybe Text)
    , _lgrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGrantsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrsTruncated'
--
-- * 'lgrsGrants'
--
-- * 'lgrsNextMarker'
--
-- * 'lgrsResponseStatus'
listGrantsResponse
    :: Int -- ^ 'lgrsResponseStatus'
    -> ListGrantsResponse
listGrantsResponse pResponseStatus_ =
    ListGrantsResponse'
    { _lgrsTruncated = Nothing
    , _lgrsGrants = Nothing
    , _lgrsNextMarker = Nothing
    , _lgrsResponseStatus = pResponseStatus_
    }

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more grants in the
-- list.
lgrsTruncated :: Lens' ListGrantsResponse (Maybe Bool)
lgrsTruncated = lens _lgrsTruncated (\ s a -> s{_lgrsTruncated = a});

-- | A list of grants.
lgrsGrants :: Lens' ListGrantsResponse [GrantListEntry]
lgrsGrants = lens _lgrsGrants (\ s a -> s{_lgrsGrants = a}) . _Default . _Coerce;

-- | If 'Truncated' is true, this value is present and contains the value to
-- use for the 'Marker' request parameter in a subsequent pagination
-- request.
lgrsNextMarker :: Lens' ListGrantsResponse (Maybe Text)
lgrsNextMarker = lens _lgrsNextMarker (\ s a -> s{_lgrsNextMarker = a});

-- | The response status code.
lgrsResponseStatus :: Lens' ListGrantsResponse Int
lgrsResponseStatus = lens _lgrsResponseStatus (\ s a -> s{_lgrsResponseStatus = a});
