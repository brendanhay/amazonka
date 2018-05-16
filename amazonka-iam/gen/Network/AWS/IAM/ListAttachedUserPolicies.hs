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
-- Module      : Network.AWS.IAM.ListAttachedUserPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified IAM user.
--
--
-- An IAM user can also have inline policies embedded with it. To list the inline policies for a user, use the 'ListUserPolicies' API. For information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. You can use the @PathPrefix@ parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified group (or none that match the specified path prefix), the operation returns an empty list.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAttachedUserPolicies
    (
    -- * Creating a Request
      listAttachedUserPolicies
    , ListAttachedUserPolicies
    -- * Request Lenses
    , laupPathPrefix
    , laupMarker
    , laupMaxItems
    , laupUserName

    -- * Destructuring the Response
    , listAttachedUserPoliciesResponse
    , ListAttachedUserPoliciesResponse
    -- * Response Lenses
    , lauprsAttachedPolicies
    , lauprsMarker
    , lauprsIsTruncated
    , lauprsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAttachedUserPolicies' smart constructor.
data ListAttachedUserPolicies = ListAttachedUserPolicies'
  { _laupPathPrefix :: !(Maybe Text)
  , _laupMarker     :: !(Maybe Text)
  , _laupMaxItems   :: !(Maybe Nat)
  , _laupUserName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttachedUserPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laupPathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'laupMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'laupMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'laupUserName' - The name (friendly name, not ARN) of the user to list attached policies for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
listAttachedUserPolicies
    :: Text -- ^ 'laupUserName'
    -> ListAttachedUserPolicies
listAttachedUserPolicies pUserName_ =
  ListAttachedUserPolicies'
    { _laupPathPrefix = Nothing
    , _laupMarker = Nothing
    , _laupMaxItems = Nothing
    , _laupUserName = pUserName_
    }


-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
laupPathPrefix :: Lens' ListAttachedUserPolicies (Maybe Text)
laupPathPrefix = lens _laupPathPrefix (\ s a -> s{_laupPathPrefix = a})

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
laupMarker :: Lens' ListAttachedUserPolicies (Maybe Text)
laupMarker = lens _laupMarker (\ s a -> s{_laupMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
laupMaxItems :: Lens' ListAttachedUserPolicies (Maybe Natural)
laupMaxItems = lens _laupMaxItems (\ s a -> s{_laupMaxItems = a}) . mapping _Nat

-- | The name (friendly name, not ARN) of the user to list attached policies for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
laupUserName :: Lens' ListAttachedUserPolicies Text
laupUserName = lens _laupUserName (\ s a -> s{_laupUserName = a})

instance AWSPager ListAttachedUserPolicies where
        page rq rs
          | stop (rs ^. lauprsIsTruncated) = Nothing
          | isNothing (rs ^. lauprsMarker) = Nothing
          | otherwise =
            Just $ rq & laupMarker .~ rs ^. lauprsMarker

instance AWSRequest ListAttachedUserPolicies where
        type Rs ListAttachedUserPolicies =
             ListAttachedUserPoliciesResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListAttachedUserPoliciesResult"
              (\ s h x ->
                 ListAttachedUserPoliciesResponse' <$>
                   (x .@? "AttachedPolicies" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance Hashable ListAttachedUserPolicies where

instance NFData ListAttachedUserPolicies where

instance ToHeaders ListAttachedUserPolicies where
        toHeaders = const mempty

instance ToPath ListAttachedUserPolicies where
        toPath = const "/"

instance ToQuery ListAttachedUserPolicies where
        toQuery ListAttachedUserPolicies'{..}
          = mconcat
              ["Action" =:
                 ("ListAttachedUserPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _laupPathPrefix,
               "Marker" =: _laupMarker, "MaxItems" =: _laupMaxItems,
               "UserName" =: _laupUserName]

-- | Contains the response to a successful 'ListAttachedUserPolicies' request.
--
--
--
-- /See:/ 'listAttachedUserPoliciesResponse' smart constructor.
data ListAttachedUserPoliciesResponse = ListAttachedUserPoliciesResponse'
  { _lauprsAttachedPolicies :: !(Maybe [AttachedPolicy])
  , _lauprsMarker           :: !(Maybe Text)
  , _lauprsIsTruncated      :: !(Maybe Bool)
  , _lauprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttachedUserPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lauprsAttachedPolicies' - A list of the attached policies.
--
-- * 'lauprsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lauprsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lauprsResponseStatus' - -- | The response status code.
listAttachedUserPoliciesResponse
    :: Int -- ^ 'lauprsResponseStatus'
    -> ListAttachedUserPoliciesResponse
listAttachedUserPoliciesResponse pResponseStatus_ =
  ListAttachedUserPoliciesResponse'
    { _lauprsAttachedPolicies = Nothing
    , _lauprsMarker = Nothing
    , _lauprsIsTruncated = Nothing
    , _lauprsResponseStatus = pResponseStatus_
    }


-- | A list of the attached policies.
lauprsAttachedPolicies :: Lens' ListAttachedUserPoliciesResponse [AttachedPolicy]
lauprsAttachedPolicies = lens _lauprsAttachedPolicies (\ s a -> s{_lauprsAttachedPolicies = a}) . _Default . _Coerce

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lauprsMarker :: Lens' ListAttachedUserPoliciesResponse (Maybe Text)
lauprsMarker = lens _lauprsMarker (\ s a -> s{_lauprsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lauprsIsTruncated :: Lens' ListAttachedUserPoliciesResponse (Maybe Bool)
lauprsIsTruncated = lens _lauprsIsTruncated (\ s a -> s{_lauprsIsTruncated = a})

-- | -- | The response status code.
lauprsResponseStatus :: Lens' ListAttachedUserPoliciesResponse Int
lauprsResponseStatus = lens _lauprsResponseStatus (\ s a -> s{_lauprsResponseStatus = a})

instance NFData ListAttachedUserPoliciesResponse
         where
