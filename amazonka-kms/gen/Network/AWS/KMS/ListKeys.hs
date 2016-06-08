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
-- Module      : Network.AWS.KMS.ListKeys
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the customer master keys.
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListKeys
    (
    -- * Creating a Request
      listKeys
    , ListKeys
    -- * Request Lenses
    , lkMarker
    , lkLimit

    -- * Destructuring the Response
    , listKeysResponse
    , ListKeysResponse
    -- * Response Lenses
    , lkrsTruncated
    , lkrsKeys
    , lkrsNextMarker
    , lkrsResponseStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listKeys' smart constructor.
data ListKeys = ListKeys'
    { _lkMarker :: !(Maybe Text)
    , _lkLimit  :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lkMarker'
--
-- * 'lkLimit'
listKeys
    :: ListKeys
listKeys =
    ListKeys'
    { _lkMarker = Nothing
    , _lkLimit = Nothing
    }

-- | Use this parameter only when paginating results and only in a subsequent request after you receive a response with truncated results. Set it to the value of 'NextMarker' from the response you just received.
lkMarker :: Lens' ListKeys (Maybe Text)
lkMarker = lens _lkMarker (\ s a -> s{_lkMarker = a});

-- | When paginating results, specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the 'Truncated' element in the response is set to true.
--
-- This value is optional. If you include a value, it must be between 1 and 1000, inclusive. If you do not include a value, it defaults to 100.
lkLimit :: Lens' ListKeys (Maybe Natural)
lkLimit = lens _lkLimit (\ s a -> s{_lkLimit = a}) . mapping _Nat;

instance AWSPager ListKeys where
        page rq rs
          | stop (rs ^. lkrsTruncated) = Nothing
          | isNothing (rs ^. lkrsNextMarker) = Nothing
          | otherwise =
            Just $ rq & lkMarker .~ rs ^. lkrsNextMarker

instance AWSRequest ListKeys where
        type Rs ListKeys = ListKeysResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 ListKeysResponse' <$>
                   (x .?> "Truncated") <*> (x .?> "Keys" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListKeys

instance NFData ListKeys

instance ToHeaders ListKeys where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ListKeys" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListKeys where
        toJSON ListKeys'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _lkMarker,
                  ("Limit" .=) <$> _lkLimit])

instance ToPath ListKeys where
        toPath = const "/"

instance ToQuery ListKeys where
        toQuery = const mempty

-- | /See:/ 'listKeysResponse' smart constructor.
data ListKeysResponse = ListKeysResponse'
    { _lkrsTruncated      :: !(Maybe Bool)
    , _lkrsKeys           :: !(Maybe [KeyListEntry])
    , _lkrsNextMarker     :: !(Maybe Text)
    , _lkrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListKeysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lkrsTruncated'
--
-- * 'lkrsKeys'
--
-- * 'lkrsNextMarker'
--
-- * 'lkrsResponseStatus'
listKeysResponse
    :: Int -- ^ 'lkrsResponseStatus'
    -> ListKeysResponse
listKeysResponse pResponseStatus_ =
    ListKeysResponse'
    { _lkrsTruncated = Nothing
    , _lkrsKeys = Nothing
    , _lkrsNextMarker = Nothing
    , _lkrsResponseStatus = pResponseStatus_
    }

-- | A flag that indicates whether there are more items in the list. If your results were truncated, you can use the 'Marker' parameter to make a subsequent pagination request to retrieve more items in the list.
lkrsTruncated :: Lens' ListKeysResponse (Maybe Bool)
lkrsTruncated = lens _lkrsTruncated (\ s a -> s{_lkrsTruncated = a});

-- | A list of keys.
lkrsKeys :: Lens' ListKeysResponse [KeyListEntry]
lkrsKeys = lens _lkrsKeys (\ s a -> s{_lkrsKeys = a}) . _Default . _Coerce;

-- | When 'Truncated' is true, this value is present and contains the value to use for the 'Marker' parameter in a subsequent pagination request.
lkrsNextMarker :: Lens' ListKeysResponse (Maybe Text)
lkrsNextMarker = lens _lkrsNextMarker (\ s a -> s{_lkrsNextMarker = a});

-- | The response status code.
lkrsResponseStatus :: Lens' ListKeysResponse Int
lkrsResponseStatus = lens _lkrsResponseStatus (\ s a -> s{_lkrsResponseStatus = a});

instance NFData ListKeysResponse
