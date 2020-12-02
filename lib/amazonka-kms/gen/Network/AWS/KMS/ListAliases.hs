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
-- Module      : Network.AWS.KMS.ListAliases
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all aliases in the caller's AWS account and region. You cannot list aliases in other accounts. For more information about aliases, see 'CreateAlias' .
--
--
-- The response might include several aliases that do not have a @TargetKeyId@ field because they are not associated with a CMK. These are predefined aliases that are reserved for CMKs managed by AWS services. If an alias is not associated with a CMK, the alias does not count against the <http://docs.aws.amazon.com/kms/latest/developerguide/limits.html#aliases-limit alias limit> for your account.
--
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListAliases
    (
    -- * Creating a Request
      listAliases
    , ListAliases
    -- * Request Lenses
    , laMarker
    , laLimit

    -- * Destructuring the Response
    , listAliasesResponse
    , ListAliasesResponse
    -- * Response Lenses
    , larsTruncated
    , larsAliases
    , larsNextMarker
    , larsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAliases' smart constructor.
data ListAliases = ListAliases'
  { _laMarker :: !(Maybe Text)
  , _laLimit  :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laMarker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- * 'laLimit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer. This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
listAliases
    :: ListAliases
listAliases = ListAliases' {_laMarker = Nothing, _laLimit = Nothing}


-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
laMarker :: Lens' ListAliases (Maybe Text)
laMarker = lens _laMarker (\ s a -> s{_laMarker = a})

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer. This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
laLimit :: Lens' ListAliases (Maybe Natural)
laLimit = lens _laLimit (\ s a -> s{_laLimit = a}) . mapping _Nat

instance AWSPager ListAliases where
        page rq rs
          | stop (rs ^. larsTruncated) = Nothing
          | isNothing (rs ^. larsNextMarker) = Nothing
          | otherwise =
            Just $ rq & laMarker .~ rs ^. larsNextMarker

instance AWSRequest ListAliases where
        type Rs ListAliases = ListAliasesResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 ListAliasesResponse' <$>
                   (x .?> "Truncated") <*> (x .?> "Aliases" .!@ mempty)
                     <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListAliases where

instance NFData ListAliases where

instance ToHeaders ListAliases where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ListAliases" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAliases where
        toJSON ListAliases'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _laMarker,
                  ("Limit" .=) <$> _laLimit])

instance ToPath ListAliases where
        toPath = const "/"

instance ToQuery ListAliases where
        toQuery = const mempty

-- | /See:/ 'listAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { _larsTruncated      :: !(Maybe Bool)
  , _larsAliases        :: !(Maybe [AliasListEntry])
  , _larsNextMarker     :: !(Maybe Text)
  , _larsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAliasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsTruncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in this response to the @Marker@ parameter in a subsequent request.
--
-- * 'larsAliases' - A list of aliases.
--
-- * 'larsNextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- * 'larsResponseStatus' - -- | The response status code.
listAliasesResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAliasesResponse
listAliasesResponse pResponseStatus_ =
  ListAliasesResponse'
    { _larsTruncated = Nothing
    , _larsAliases = Nothing
    , _larsNextMarker = Nothing
    , _larsResponseStatus = pResponseStatus_
    }


-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in this response to the @Marker@ parameter in a subsequent request.
larsTruncated :: Lens' ListAliasesResponse (Maybe Bool)
larsTruncated = lens _larsTruncated (\ s a -> s{_larsTruncated = a})

-- | A list of aliases.
larsAliases :: Lens' ListAliasesResponse [AliasListEntry]
larsAliases = lens _larsAliases (\ s a -> s{_larsAliases = a}) . _Default . _Coerce

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
larsNextMarker :: Lens' ListAliasesResponse (Maybe Text)
larsNextMarker = lens _larsNextMarker (\ s a -> s{_larsNextMarker = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAliasesResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

instance NFData ListAliasesResponse where
