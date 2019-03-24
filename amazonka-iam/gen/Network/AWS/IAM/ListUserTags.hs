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
-- Module      : Network.AWS.IAM.ListUserTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified user. The returned list of tags is sorted by tag key. For more information about tagging, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
--
module Network.AWS.IAM.ListUserTags
    (
    -- * Creating a Request
      listUserTags
    , ListUserTags
    -- * Request Lenses
    , lutMarker
    , lutMaxItems
    , lutUserName

    -- * Destructuring the Response
    , listUserTagsResponse
    , ListUserTagsResponse
    -- * Response Lenses
    , lutrsMarker
    , lutrsIsTruncated
    , lutrsResponseStatus
    , lutrsTags
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listUserTags' smart constructor.
data ListUserTags = ListUserTags'
  { _lutMarker   :: !(Maybe Text)
  , _lutMaxItems :: !(Maybe Nat)
  , _lutUserName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lutMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response to indicate where the next call should start.
--
-- * 'lutMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'lutUserName' - The name of the IAM user whose tags you want to see. This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
listUserTags
    :: Text -- ^ 'lutUserName'
    -> ListUserTags
listUserTags pUserName_ =
  ListUserTags'
    {_lutMarker = Nothing, _lutMaxItems = Nothing, _lutUserName = pUserName_}


-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response to indicate where the next call should start.
lutMarker :: Lens' ListUserTags (Maybe Text)
lutMarker = lens _lutMarker (\ s a -> s{_lutMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lutMaxItems :: Lens' ListUserTags (Maybe Natural)
lutMaxItems = lens _lutMaxItems (\ s a -> s{_lutMaxItems = a}) . mapping _Nat

-- | The name of the IAM user whose tags you want to see. This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
lutUserName :: Lens' ListUserTags Text
lutUserName = lens _lutUserName (\ s a -> s{_lutUserName = a})

instance AWSRequest ListUserTags where
        type Rs ListUserTags = ListUserTagsResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListUserTagsResult"
              (\ s h x ->
                 ListUserTagsResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "Tags" .!@ mempty >>= parseXMLList "member"))

instance Hashable ListUserTags where

instance NFData ListUserTags where

instance ToHeaders ListUserTags where
        toHeaders = const mempty

instance ToPath ListUserTags where
        toPath = const "/"

instance ToQuery ListUserTags where
        toQuery ListUserTags'{..}
          = mconcat
              ["Action" =: ("ListUserTags" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _lutMarker, "MaxItems" =: _lutMaxItems,
               "UserName" =: _lutUserName]

-- | /See:/ 'listUserTagsResponse' smart constructor.
data ListUserTagsResponse = ListUserTagsResponse'
  { _lutrsMarker         :: !(Maybe Text)
  , _lutrsIsTruncated    :: !(Maybe Bool)
  , _lutrsResponseStatus :: !Int
  , _lutrsTags           :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lutrsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lutrsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lutrsResponseStatus' - -- | The response status code.
--
-- * 'lutrsTags' - The list of tags that are currently attached to the user. Each tag consists of a key name and an associated value. If no tags are attached to the specified user, the response contains an empty list.
listUserTagsResponse
    :: Int -- ^ 'lutrsResponseStatus'
    -> ListUserTagsResponse
listUserTagsResponse pResponseStatus_ =
  ListUserTagsResponse'
    { _lutrsMarker = Nothing
    , _lutrsIsTruncated = Nothing
    , _lutrsResponseStatus = pResponseStatus_
    , _lutrsTags = mempty
    }


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lutrsMarker :: Lens' ListUserTagsResponse (Maybe Text)
lutrsMarker = lens _lutrsMarker (\ s a -> s{_lutrsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
lutrsIsTruncated :: Lens' ListUserTagsResponse (Maybe Bool)
lutrsIsTruncated = lens _lutrsIsTruncated (\ s a -> s{_lutrsIsTruncated = a})

-- | -- | The response status code.
lutrsResponseStatus :: Lens' ListUserTagsResponse Int
lutrsResponseStatus = lens _lutrsResponseStatus (\ s a -> s{_lutrsResponseStatus = a})

-- | The list of tags that are currently attached to the user. Each tag consists of a key name and an associated value. If no tags are attached to the specified user, the response contains an empty list.
lutrsTags :: Lens' ListUserTagsResponse [Tag]
lutrsTags = lens _lutrsTags (\ s a -> s{_lutrsTags = a}) . _Coerce

instance NFData ListUserTagsResponse where
