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
-- Module      : Network.AWS.IAM.ListAttachedGroupPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified IAM group.
--
--
-- An IAM group can also have inline policies embedded with it. To list the inline policies for a group, use the 'ListGroupPolicies' API. For information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. You can use the @PathPrefix@ parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified group (or none that match the specified path prefix), the operation returns an empty list.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAttachedGroupPolicies
    (
    -- * Creating a Request
      listAttachedGroupPolicies
    , ListAttachedGroupPolicies
    -- * Request Lenses
    , lagpPathPrefix
    , lagpMarker
    , lagpMaxItems
    , lagpGroupName

    -- * Destructuring the Response
    , listAttachedGroupPoliciesResponse
    , ListAttachedGroupPoliciesResponse
    -- * Response Lenses
    , lagprsAttachedPolicies
    , lagprsMarker
    , lagprsIsTruncated
    , lagprsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAttachedGroupPolicies' smart constructor.
data ListAttachedGroupPolicies = ListAttachedGroupPolicies'
  { _lagpPathPrefix :: !(Maybe Text)
  , _lagpMarker     :: !(Maybe Text)
  , _lagpMaxItems   :: !(Maybe Nat)
  , _lagpGroupName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttachedGroupPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lagpPathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'lagpMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lagpMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'lagpGroupName' - The name (friendly name, not ARN) of the group to list attached policies for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
listAttachedGroupPolicies
    :: Text -- ^ 'lagpGroupName'
    -> ListAttachedGroupPolicies
listAttachedGroupPolicies pGroupName_ =
  ListAttachedGroupPolicies'
    { _lagpPathPrefix = Nothing
    , _lagpMarker = Nothing
    , _lagpMaxItems = Nothing
    , _lagpGroupName = pGroupName_
    }


-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
lagpPathPrefix :: Lens' ListAttachedGroupPolicies (Maybe Text)
lagpPathPrefix = lens _lagpPathPrefix (\ s a -> s{_lagpPathPrefix = a})

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lagpMarker :: Lens' ListAttachedGroupPolicies (Maybe Text)
lagpMarker = lens _lagpMarker (\ s a -> s{_lagpMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lagpMaxItems :: Lens' ListAttachedGroupPolicies (Maybe Natural)
lagpMaxItems = lens _lagpMaxItems (\ s a -> s{_lagpMaxItems = a}) . mapping _Nat

-- | The name (friendly name, not ARN) of the group to list attached policies for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
lagpGroupName :: Lens' ListAttachedGroupPolicies Text
lagpGroupName = lens _lagpGroupName (\ s a -> s{_lagpGroupName = a})

instance AWSPager ListAttachedGroupPolicies where
        page rq rs
          | stop (rs ^. lagprsIsTruncated) = Nothing
          | isNothing (rs ^. lagprsMarker) = Nothing
          | otherwise =
            Just $ rq & lagpMarker .~ rs ^. lagprsMarker

instance AWSRequest ListAttachedGroupPolicies where
        type Rs ListAttachedGroupPolicies =
             ListAttachedGroupPoliciesResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListAttachedGroupPoliciesResult"
              (\ s h x ->
                 ListAttachedGroupPoliciesResponse' <$>
                   (x .@? "AttachedPolicies" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance Hashable ListAttachedGroupPolicies where

instance NFData ListAttachedGroupPolicies where

instance ToHeaders ListAttachedGroupPolicies where
        toHeaders = const mempty

instance ToPath ListAttachedGroupPolicies where
        toPath = const "/"

instance ToQuery ListAttachedGroupPolicies where
        toQuery ListAttachedGroupPolicies'{..}
          = mconcat
              ["Action" =:
                 ("ListAttachedGroupPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lagpPathPrefix,
               "Marker" =: _lagpMarker, "MaxItems" =: _lagpMaxItems,
               "GroupName" =: _lagpGroupName]

-- | Contains the response to a successful 'ListAttachedGroupPolicies' request.
--
--
--
-- /See:/ 'listAttachedGroupPoliciesResponse' smart constructor.
data ListAttachedGroupPoliciesResponse = ListAttachedGroupPoliciesResponse'
  { _lagprsAttachedPolicies :: !(Maybe [AttachedPolicy])
  , _lagprsMarker           :: !(Maybe Text)
  , _lagprsIsTruncated      :: !(Maybe Bool)
  , _lagprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttachedGroupPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lagprsAttachedPolicies' - A list of the attached policies.
--
-- * 'lagprsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lagprsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lagprsResponseStatus' - -- | The response status code.
listAttachedGroupPoliciesResponse
    :: Int -- ^ 'lagprsResponseStatus'
    -> ListAttachedGroupPoliciesResponse
listAttachedGroupPoliciesResponse pResponseStatus_ =
  ListAttachedGroupPoliciesResponse'
    { _lagprsAttachedPolicies = Nothing
    , _lagprsMarker = Nothing
    , _lagprsIsTruncated = Nothing
    , _lagprsResponseStatus = pResponseStatus_
    }


-- | A list of the attached policies.
lagprsAttachedPolicies :: Lens' ListAttachedGroupPoliciesResponse [AttachedPolicy]
lagprsAttachedPolicies = lens _lagprsAttachedPolicies (\ s a -> s{_lagprsAttachedPolicies = a}) . _Default . _Coerce

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lagprsMarker :: Lens' ListAttachedGroupPoliciesResponse (Maybe Text)
lagprsMarker = lens _lagprsMarker (\ s a -> s{_lagprsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lagprsIsTruncated :: Lens' ListAttachedGroupPoliciesResponse (Maybe Bool)
lagprsIsTruncated = lens _lagprsIsTruncated (\ s a -> s{_lagprsIsTruncated = a})

-- | -- | The response status code.
lagprsResponseStatus :: Lens' ListAttachedGroupPoliciesResponse Int
lagprsResponseStatus = lens _lagprsResponseStatus (\ s a -> s{_lagprsResponseStatus = a})

instance NFData ListAttachedGroupPoliciesResponse
         where
