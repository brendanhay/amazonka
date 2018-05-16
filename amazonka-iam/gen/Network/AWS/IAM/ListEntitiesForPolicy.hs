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
-- Module      : Network.AWS.IAM.ListEntitiesForPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all IAM users, groups, and roles that the specified managed policy is attached to.
--
--
-- You can use the optional @EntityFilter@ parameter to limit the results to a particular type of entity (users, groups, or roles). For example, to list only the roles that are attached to the specified policy, set @EntityFilter@ to @Role@ .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListEntitiesForPolicy
    (
    -- * Creating a Request
      listEntitiesForPolicy
    , ListEntitiesForPolicy
    -- * Request Lenses
    , lefpPathPrefix
    , lefpEntityFilter
    , lefpMarker
    , lefpMaxItems
    , lefpPolicyARN

    -- * Destructuring the Response
    , listEntitiesForPolicyResponse
    , ListEntitiesForPolicyResponse
    -- * Response Lenses
    , lefprsPolicyGroups
    , lefprsPolicyRoles
    , lefprsMarker
    , lefprsPolicyUsers
    , lefprsIsTruncated
    , lefprsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listEntitiesForPolicy' smart constructor.
data ListEntitiesForPolicy = ListEntitiesForPolicy'
  { _lefpPathPrefix   :: !(Maybe Text)
  , _lefpEntityFilter :: !(Maybe EntityType)
  , _lefpMarker       :: !(Maybe Text)
  , _lefpMaxItems     :: !(Maybe Nat)
  , _lefpPolicyARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEntitiesForPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lefpPathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all entities. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'lefpEntityFilter' - The entity type to use for filtering the results. For example, when @EntityFilter@ is @Role@ , only the roles that are attached to the specified policy are returned. This parameter is optional. If it is not included, all attached entities (users, groups, and roles) are returned. The argument for this parameter must be one of the valid values listed below.
--
-- * 'lefpMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lefpMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'lefpPolicyARN' - The Amazon Resource Name (ARN) of the IAM policy for which you want the versions. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
listEntitiesForPolicy
    :: Text -- ^ 'lefpPolicyARN'
    -> ListEntitiesForPolicy
listEntitiesForPolicy pPolicyARN_ =
  ListEntitiesForPolicy'
    { _lefpPathPrefix = Nothing
    , _lefpEntityFilter = Nothing
    , _lefpMarker = Nothing
    , _lefpMaxItems = Nothing
    , _lefpPolicyARN = pPolicyARN_
    }


-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all entities. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
lefpPathPrefix :: Lens' ListEntitiesForPolicy (Maybe Text)
lefpPathPrefix = lens _lefpPathPrefix (\ s a -> s{_lefpPathPrefix = a})

-- | The entity type to use for filtering the results. For example, when @EntityFilter@ is @Role@ , only the roles that are attached to the specified policy are returned. This parameter is optional. If it is not included, all attached entities (users, groups, and roles) are returned. The argument for this parameter must be one of the valid values listed below.
lefpEntityFilter :: Lens' ListEntitiesForPolicy (Maybe EntityType)
lefpEntityFilter = lens _lefpEntityFilter (\ s a -> s{_lefpEntityFilter = a})

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lefpMarker :: Lens' ListEntitiesForPolicy (Maybe Text)
lefpMarker = lens _lefpMarker (\ s a -> s{_lefpMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lefpMaxItems :: Lens' ListEntitiesForPolicy (Maybe Natural)
lefpMaxItems = lens _lefpMaxItems (\ s a -> s{_lefpMaxItems = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the IAM policy for which you want the versions. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
lefpPolicyARN :: Lens' ListEntitiesForPolicy Text
lefpPolicyARN = lens _lefpPolicyARN (\ s a -> s{_lefpPolicyARN = a})

instance AWSPager ListEntitiesForPolicy where
        page rq rs
          | stop (rs ^. lefprsIsTruncated) = Nothing
          | isNothing (rs ^. lefprsMarker) = Nothing
          | otherwise =
            Just $ rq & lefpMarker .~ rs ^. lefprsMarker

instance AWSRequest ListEntitiesForPolicy where
        type Rs ListEntitiesForPolicy =
             ListEntitiesForPolicyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListEntitiesForPolicyResult"
              (\ s h x ->
                 ListEntitiesForPolicyResponse' <$>
                   (x .@? "PolicyGroups" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*>
                     (x .@? "PolicyRoles" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*>
                     (x .@? "PolicyUsers" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance Hashable ListEntitiesForPolicy where

instance NFData ListEntitiesForPolicy where

instance ToHeaders ListEntitiesForPolicy where
        toHeaders = const mempty

instance ToPath ListEntitiesForPolicy where
        toPath = const "/"

instance ToQuery ListEntitiesForPolicy where
        toQuery ListEntitiesForPolicy'{..}
          = mconcat
              ["Action" =: ("ListEntitiesForPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lefpPathPrefix,
               "EntityFilter" =: _lefpEntityFilter,
               "Marker" =: _lefpMarker, "MaxItems" =: _lefpMaxItems,
               "PolicyArn" =: _lefpPolicyARN]

-- | Contains the response to a successful 'ListEntitiesForPolicy' request.
--
--
--
-- /See:/ 'listEntitiesForPolicyResponse' smart constructor.
data ListEntitiesForPolicyResponse = ListEntitiesForPolicyResponse'
  { _lefprsPolicyGroups   :: !(Maybe [PolicyGroup])
  , _lefprsPolicyRoles    :: !(Maybe [PolicyRole])
  , _lefprsMarker         :: !(Maybe Text)
  , _lefprsPolicyUsers    :: !(Maybe [PolicyUser])
  , _lefprsIsTruncated    :: !(Maybe Bool)
  , _lefprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEntitiesForPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lefprsPolicyGroups' - A list of IAM groups that the policy is attached to.
--
-- * 'lefprsPolicyRoles' - A list of IAM roles that the policy is attached to.
--
-- * 'lefprsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lefprsPolicyUsers' - A list of IAM users that the policy is attached to.
--
-- * 'lefprsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lefprsResponseStatus' - -- | The response status code.
listEntitiesForPolicyResponse
    :: Int -- ^ 'lefprsResponseStatus'
    -> ListEntitiesForPolicyResponse
listEntitiesForPolicyResponse pResponseStatus_ =
  ListEntitiesForPolicyResponse'
    { _lefprsPolicyGroups = Nothing
    , _lefprsPolicyRoles = Nothing
    , _lefprsMarker = Nothing
    , _lefprsPolicyUsers = Nothing
    , _lefprsIsTruncated = Nothing
    , _lefprsResponseStatus = pResponseStatus_
    }


-- | A list of IAM groups that the policy is attached to.
lefprsPolicyGroups :: Lens' ListEntitiesForPolicyResponse [PolicyGroup]
lefprsPolicyGroups = lens _lefprsPolicyGroups (\ s a -> s{_lefprsPolicyGroups = a}) . _Default . _Coerce

-- | A list of IAM roles that the policy is attached to.
lefprsPolicyRoles :: Lens' ListEntitiesForPolicyResponse [PolicyRole]
lefprsPolicyRoles = lens _lefprsPolicyRoles (\ s a -> s{_lefprsPolicyRoles = a}) . _Default . _Coerce

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lefprsMarker :: Lens' ListEntitiesForPolicyResponse (Maybe Text)
lefprsMarker = lens _lefprsMarker (\ s a -> s{_lefprsMarker = a})

-- | A list of IAM users that the policy is attached to.
lefprsPolicyUsers :: Lens' ListEntitiesForPolicyResponse [PolicyUser]
lefprsPolicyUsers = lens _lefprsPolicyUsers (\ s a -> s{_lefprsPolicyUsers = a}) . _Default . _Coerce

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lefprsIsTruncated :: Lens' ListEntitiesForPolicyResponse (Maybe Bool)
lefprsIsTruncated = lens _lefprsIsTruncated (\ s a -> s{_lefprsIsTruncated = a})

-- | -- | The response status code.
lefprsResponseStatus :: Lens' ListEntitiesForPolicyResponse Int
lefprsResponseStatus = lens _lefprsResponseStatus (\ s a -> s{_lefprsResponseStatus = a})

instance NFData ListEntitiesForPolicyResponse where
