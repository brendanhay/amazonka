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
-- Module      : Network.AWS.IAM.ListRoleTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified role. The returned list of tags is sorted by tag key. For more information about tagging, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
--
module Network.AWS.IAM.ListRoleTags
    (
    -- * Creating a Request
      listRoleTags
    , ListRoleTags
    -- * Request Lenses
    , lrtMarker
    , lrtMaxItems
    , lrtRoleName

    -- * Destructuring the Response
    , listRoleTagsResponse
    , ListRoleTagsResponse
    -- * Response Lenses
    , lrtrsMarker
    , lrtrsIsTruncated
    , lrtrsResponseStatus
    , lrtrsTags
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRoleTags' smart constructor.
data ListRoleTags = ListRoleTags'
  { _lrtMarker   :: !(Maybe Text)
  , _lrtMaxItems :: !(Maybe Nat)
  , _lrtRoleName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRoleTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrtMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response to indicate where the next call should start.
--
-- * 'lrtMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'lrtRoleName' - The name of the IAM role for which you want to see the list of tags. This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
listRoleTags
    :: Text -- ^ 'lrtRoleName'
    -> ListRoleTags
listRoleTags pRoleName_ =
  ListRoleTags'
    {_lrtMarker = Nothing, _lrtMaxItems = Nothing, _lrtRoleName = pRoleName_}


-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response to indicate where the next call should start.
lrtMarker :: Lens' ListRoleTags (Maybe Text)
lrtMarker = lens _lrtMarker (\ s a -> s{_lrtMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lrtMaxItems :: Lens' ListRoleTags (Maybe Natural)
lrtMaxItems = lens _lrtMaxItems (\ s a -> s{_lrtMaxItems = a}) . mapping _Nat

-- | The name of the IAM role for which you want to see the list of tags. This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
lrtRoleName :: Lens' ListRoleTags Text
lrtRoleName = lens _lrtRoleName (\ s a -> s{_lrtRoleName = a})

instance AWSRequest ListRoleTags where
        type Rs ListRoleTags = ListRoleTagsResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListRoleTagsResult"
              (\ s h x ->
                 ListRoleTagsResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "Tags" .!@ mempty >>= parseXMLList "member"))

instance Hashable ListRoleTags where

instance NFData ListRoleTags where

instance ToHeaders ListRoleTags where
        toHeaders = const mempty

instance ToPath ListRoleTags where
        toPath = const "/"

instance ToQuery ListRoleTags where
        toQuery ListRoleTags'{..}
          = mconcat
              ["Action" =: ("ListRoleTags" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _lrtMarker, "MaxItems" =: _lrtMaxItems,
               "RoleName" =: _lrtRoleName]

-- | /See:/ 'listRoleTagsResponse' smart constructor.
data ListRoleTagsResponse = ListRoleTagsResponse'
  { _lrtrsMarker         :: !(Maybe Text)
  , _lrtrsIsTruncated    :: !(Maybe Bool)
  , _lrtrsResponseStatus :: !Int
  , _lrtrsTags           :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRoleTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrtrsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lrtrsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lrtrsResponseStatus' - -- | The response status code.
--
-- * 'lrtrsTags' - The list of tags currently that is attached to the role. Each tag consists of a key name and an associated value. If no tags are attached to the specified role, the response contains an empty list.
listRoleTagsResponse
    :: Int -- ^ 'lrtrsResponseStatus'
    -> ListRoleTagsResponse
listRoleTagsResponse pResponseStatus_ =
  ListRoleTagsResponse'
    { _lrtrsMarker = Nothing
    , _lrtrsIsTruncated = Nothing
    , _lrtrsResponseStatus = pResponseStatus_
    , _lrtrsTags = mempty
    }


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lrtrsMarker :: Lens' ListRoleTagsResponse (Maybe Text)
lrtrsMarker = lens _lrtrsMarker (\ s a -> s{_lrtrsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
lrtrsIsTruncated :: Lens' ListRoleTagsResponse (Maybe Bool)
lrtrsIsTruncated = lens _lrtrsIsTruncated (\ s a -> s{_lrtrsIsTruncated = a})

-- | -- | The response status code.
lrtrsResponseStatus :: Lens' ListRoleTagsResponse Int
lrtrsResponseStatus = lens _lrtrsResponseStatus (\ s a -> s{_lrtrsResponseStatus = a})

-- | The list of tags currently that is attached to the role. Each tag consists of a key name and an associated value. If no tags are attached to the specified role, the response contains an empty list.
lrtrsTags :: Lens' ListRoleTagsResponse [Tag]
lrtrsTags = lens _lrtrsTags (\ s a -> s{_lrtrsTags = a}) . _Coerce

instance NFData ListRoleTagsResponse where
