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
-- Module      : Network.AWS.IAM.ListGroupsForUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM groups that the specified IAM user belongs to.
--
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListGroupsForUser
    (
    -- * Creating a Request
      listGroupsForUser
    , ListGroupsForUser
    -- * Request Lenses
    , lgfuMarker
    , lgfuMaxItems
    , lgfuUserName

    -- * Destructuring the Response
    , listGroupsForUserResponse
    , ListGroupsForUserResponse
    -- * Response Lenses
    , lgfursMarker
    , lgfursIsTruncated
    , lgfursResponseStatus
    , lgfursGroups
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGroupsForUser' smart constructor.
data ListGroupsForUser = ListGroupsForUser'
  { _lgfuMarker   :: !(Maybe Text)
  , _lgfuMaxItems :: !(Maybe Nat)
  , _lgfuUserName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupsForUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgfuMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lgfuMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'lgfuUserName' - The name of the user to list groups for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
listGroupsForUser
    :: Text -- ^ 'lgfuUserName'
    -> ListGroupsForUser
listGroupsForUser pUserName_ =
  ListGroupsForUser'
    {_lgfuMarker = Nothing, _lgfuMaxItems = Nothing, _lgfuUserName = pUserName_}


-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lgfuMarker :: Lens' ListGroupsForUser (Maybe Text)
lgfuMarker = lens _lgfuMarker (\ s a -> s{_lgfuMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lgfuMaxItems :: Lens' ListGroupsForUser (Maybe Natural)
lgfuMaxItems = lens _lgfuMaxItems (\ s a -> s{_lgfuMaxItems = a}) . mapping _Nat

-- | The name of the user to list groups for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
lgfuUserName :: Lens' ListGroupsForUser Text
lgfuUserName = lens _lgfuUserName (\ s a -> s{_lgfuUserName = a})

instance AWSPager ListGroupsForUser where
        page rq rs
          | stop (rs ^. lgfursIsTruncated) = Nothing
          | isNothing (rs ^. lgfursMarker) = Nothing
          | otherwise =
            Just $ rq & lgfuMarker .~ rs ^. lgfursMarker

instance AWSRequest ListGroupsForUser where
        type Rs ListGroupsForUser = ListGroupsForUserResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListGroupsForUserResult"
              (\ s h x ->
                 ListGroupsForUserResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "Groups" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable ListGroupsForUser where

instance NFData ListGroupsForUser where

instance ToHeaders ListGroupsForUser where
        toHeaders = const mempty

instance ToPath ListGroupsForUser where
        toPath = const "/"

instance ToQuery ListGroupsForUser where
        toQuery ListGroupsForUser'{..}
          = mconcat
              ["Action" =: ("ListGroupsForUser" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _lgfuMarker, "MaxItems" =: _lgfuMaxItems,
               "UserName" =: _lgfuUserName]

-- | Contains the response to a successful 'ListGroupsForUser' request.
--
--
--
-- /See:/ 'listGroupsForUserResponse' smart constructor.
data ListGroupsForUserResponse = ListGroupsForUserResponse'
  { _lgfursMarker         :: !(Maybe Text)
  , _lgfursIsTruncated    :: !(Maybe Bool)
  , _lgfursResponseStatus :: !Int
  , _lgfursGroups         :: ![Group]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupsForUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgfursMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lgfursIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lgfursResponseStatus' - -- | The response status code.
--
-- * 'lgfursGroups' - A list of groups.
listGroupsForUserResponse
    :: Int -- ^ 'lgfursResponseStatus'
    -> ListGroupsForUserResponse
listGroupsForUserResponse pResponseStatus_ =
  ListGroupsForUserResponse'
    { _lgfursMarker = Nothing
    , _lgfursIsTruncated = Nothing
    , _lgfursResponseStatus = pResponseStatus_
    , _lgfursGroups = mempty
    }


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lgfursMarker :: Lens' ListGroupsForUserResponse (Maybe Text)
lgfursMarker = lens _lgfursMarker (\ s a -> s{_lgfursMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lgfursIsTruncated :: Lens' ListGroupsForUserResponse (Maybe Bool)
lgfursIsTruncated = lens _lgfursIsTruncated (\ s a -> s{_lgfursIsTruncated = a})

-- | -- | The response status code.
lgfursResponseStatus :: Lens' ListGroupsForUserResponse Int
lgfursResponseStatus = lens _lgfursResponseStatus (\ s a -> s{_lgfursResponseStatus = a})

-- | A list of groups.
lgfursGroups :: Lens' ListGroupsForUserResponse [Group]
lgfursGroups = lens _lgfursGroups (\ s a -> s{_lgfursGroups = a}) . _Coerce

instance NFData ListGroupsForUserResponse where
