{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListRolePolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies that are embedded in the
-- specified role.
--
-- A role can also have managed policies attached to it. To list the
-- managed policies that are attached to a role, use
-- ListAttachedRolePolicies. For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. If there are no inline policies embedded with the specified
-- role, the action returns an empty list.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRolePolicies.html AWS API Reference> for ListRolePolicies.
module Network.AWS.IAM.ListRolePolicies
    (
    -- * Creating a Request
      ListRolePolicies
    , listRolePolicies
    -- * Request Lenses
    , lrpMaxItems
    , lrpMarker
    , lrpRoleName

    -- * Destructuring the Response
    , ListRolePoliciesResponse
    , listRolePoliciesResponse
    -- * Response Lenses
    , lrprsMarker
    , lrprsIsTruncated
    , lrprsStatus
    , lrprsPolicyNames
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listRolePolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrpMaxItems'
--
-- * 'lrpMarker'
--
-- * 'lrpRoleName'
data ListRolePolicies = ListRolePolicies'
    { _lrpMaxItems :: !(Maybe Nat)
    , _lrpMarker   :: !(Maybe Text)
    , _lrpRoleName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRolePolicies' smart constructor.
listRolePolicies :: Text -> ListRolePolicies
listRolePolicies pRoleName_ =
    ListRolePolicies'
    { _lrpMaxItems = Nothing
    , _lrpMarker = Nothing
    , _lrpRoleName = pRoleName_
    }

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lrpMaxItems :: Lens' ListRolePolicies (Maybe Natural)
lrpMaxItems = lens _lrpMaxItems (\ s a -> s{_lrpMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lrpMarker :: Lens' ListRolePolicies (Maybe Text)
lrpMarker = lens _lrpMarker (\ s a -> s{_lrpMarker = a});

-- | The name of the role to list policies for.
lrpRoleName :: Lens' ListRolePolicies Text
lrpRoleName = lens _lrpRoleName (\ s a -> s{_lrpRoleName = a});

instance AWSPager ListRolePolicies where
        page rq rs
          | stop (rs ^. lrprsIsTruncated) = Nothing
          | isNothing (rs ^. lrprsMarker) = Nothing
          | otherwise =
            Just $ rq & lrpMarker .~ rs ^. lrprsMarker

instance AWSRequest ListRolePolicies where
        type Sv ListRolePolicies = IAM
        type Rs ListRolePolicies = ListRolePoliciesResponse
        request = postQuery
        response
          = receiveXMLWrapper "ListRolePoliciesResult"
              (\ s h x ->
                 ListRolePoliciesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "PolicyNames" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListRolePolicies where
        toHeaders = const mempty

instance ToPath ListRolePolicies where
        toPath = const "/"

instance ToQuery ListRolePolicies where
        toQuery ListRolePolicies'{..}
          = mconcat
              ["Action" =: ("ListRolePolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "MaxItems" =: _lrpMaxItems, "Marker" =: _lrpMarker,
               "RoleName" =: _lrpRoleName]

-- | Contains the response to a successful ListRolePolicies request.
--
-- /See:/ 'listRolePoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrprsMarker'
--
-- * 'lrprsIsTruncated'
--
-- * 'lrprsStatus'
--
-- * 'lrprsPolicyNames'
data ListRolePoliciesResponse = ListRolePoliciesResponse'
    { _lrprsMarker      :: !(Maybe Text)
    , _lrprsIsTruncated :: !(Maybe Bool)
    , _lrprsStatus      :: !Int
    , _lrprsPolicyNames :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRolePoliciesResponse' smart constructor.
listRolePoliciesResponse :: Int -> ListRolePoliciesResponse
listRolePoliciesResponse pStatus_ =
    ListRolePoliciesResponse'
    { _lrprsMarker = Nothing
    , _lrprsIsTruncated = Nothing
    , _lrprsStatus = pStatus_
    , _lrprsPolicyNames = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lrprsMarker :: Lens' ListRolePoliciesResponse (Maybe Text)
lrprsMarker = lens _lrprsMarker (\ s a -> s{_lrprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lrprsIsTruncated :: Lens' ListRolePoliciesResponse (Maybe Bool)
lrprsIsTruncated = lens _lrprsIsTruncated (\ s a -> s{_lrprsIsTruncated = a});

-- | Undocumented member.
lrprsStatus :: Lens' ListRolePoliciesResponse Int
lrprsStatus = lens _lrprsStatus (\ s a -> s{_lrprsStatus = a});

-- | A list of policy names.
lrprsPolicyNames :: Lens' ListRolePoliciesResponse [Text]
lrprsPolicyNames = lens _lrprsPolicyNames (\ s a -> s{_lrprsPolicyNames = a}) . _Coerce;
