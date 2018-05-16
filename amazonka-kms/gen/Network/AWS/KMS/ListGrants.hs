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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all grants for the specified customer master key (CMK).
--
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the KeyId parameter.
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

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGrants' smart constructor.
data ListGrants = ListGrants'
  { _lgMarker :: !(Maybe Text)
  , _lgLimit  :: !(Maybe Nat)
  , _lgKeyId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGrants' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgMarker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- * 'lgLimit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer. This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
--
-- * 'lgKeyId' - A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
listGrants
    :: Text -- ^ 'lgKeyId'
    -> ListGrants
listGrants pKeyId_ =
  ListGrants' {_lgMarker = Nothing, _lgLimit = Nothing, _lgKeyId = pKeyId_}


-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
lgMarker :: Lens' ListGrants (Maybe Text)
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a})

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer. This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
lgLimit :: Lens' ListGrants (Maybe Natural)
lgLimit = lens _lgLimit (\ s a -> s{_lgLimit = a}) . mapping _Nat

-- | A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
lgKeyId :: Lens' ListGrants Text
lgKeyId = lens _lgKeyId (\ s a -> s{_lgKeyId = a})

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

instance Hashable ListGrants where

instance NFData ListGrants where

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
