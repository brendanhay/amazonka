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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the grants for a specified key.
--
--
--
-- This operation returns paginated results.
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
    , lgTruncated
    , lgGrants
    , lgNextMarker
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
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
-- * 'lgMarker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- * 'lgLimit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer. This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
--
-- * 'lgKeyId' - A unique identifier for the customer master key. This value can be a globally unique identifier or the fully specified ARN to a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
listGrants
    :: Text -- ^ 'lgKeyId'
    -> ListGrants
listGrants pKeyId_ =
    ListGrants'
    { _lgMarker = Nothing
    , _lgLimit = Nothing
    , _lgKeyId = pKeyId_
    }

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
lgMarker :: Lens' ListGrants (Maybe Text)
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a});

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer. This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
lgLimit :: Lens' ListGrants (Maybe Natural)
lgLimit = lens _lgLimit (\ s a -> s{_lgLimit = a}) . mapping _Nat;

-- | A unique identifier for the customer master key. This value can be a globally unique identifier or the fully specified ARN to a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
lgKeyId :: Lens' ListGrants Text
lgKeyId = lens _lgKeyId (\ s a -> s{_lgKeyId = a});

instance AWSPager ListGrants where
        page rq rs
          | stop (rs ^. lgTruncated) = Nothing
          | isNothing (rs ^. lgNextMarker) = Nothing
          | otherwise =
            Just $ rq & lgMarker .~ rs ^. lgNextMarker

instance AWSRequest ListGrants where
        type Rs ListGrants = ListGrantsResponse
        request = postJSON kms
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable ListGrants

instance NFData ListGrants

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
