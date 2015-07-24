{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListUserPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies embedded in the specified user.
--
-- A user can also have managed policies attached to it. To list the
-- managed policies that are attached to a user, use
-- ListAttachedUserPolicies. For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. If there are no inline policies embedded with the specified
-- user, the action returns an empty list.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUserPolicies.html>
module Network.AWS.IAM.ListUserPolicies
    (
    -- * Request
      ListUserPolicies
    -- ** Request constructor
    , listUserPolicies
    -- ** Request lenses
    , lupMaxItems
    , lupMarker
    , lupUserName

    -- * Response
    , ListUserPoliciesResponse
    -- ** Response constructor
    , listUserPoliciesResponse
    -- ** Response lenses
    , luprsMarker
    , luprsIsTruncated
    , luprsStatus
    , luprsPolicyNames
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listUserPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lupMaxItems'
--
-- * 'lupMarker'
--
-- * 'lupUserName'
data ListUserPolicies = ListUserPolicies'
    { _lupMaxItems :: !(Maybe Nat)
    , _lupMarker   :: !(Maybe Text)
    , _lupUserName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListUserPolicies' smart constructor.
listUserPolicies :: Text -> ListUserPolicies
listUserPolicies pUserName_ =
    ListUserPolicies'
    { _lupMaxItems = Nothing
    , _lupMarker = Nothing
    , _lupUserName = pUserName_
    }

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lupMaxItems :: Lens' ListUserPolicies (Maybe Natural)
lupMaxItems = lens _lupMaxItems (\ s a -> s{_lupMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lupMarker :: Lens' ListUserPolicies (Maybe Text)
lupMarker = lens _lupMarker (\ s a -> s{_lupMarker = a});

-- | The name of the user to list policies for.
lupUserName :: Lens' ListUserPolicies Text
lupUserName = lens _lupUserName (\ s a -> s{_lupUserName = a});

instance AWSPager ListUserPolicies where
        page rq rs
          | stop (rs ^. luprsIsTruncated) = Nothing
          | isNothing (rs ^. luprsMarker) = Nothing
          | otherwise =
            Just $ rq & lupMarker .~ rs ^. luprsMarker

instance AWSRequest ListUserPolicies where
        type Sv ListUserPolicies = IAM
        type Rs ListUserPolicies = ListUserPoliciesResponse
        request = postQuery
        response
          = receiveXMLWrapper "ListUserPoliciesResult"
              (\ s h x ->
                 ListUserPoliciesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "PolicyNames" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListUserPolicies where
        toHeaders = const mempty

instance ToPath ListUserPolicies where
        toPath = const "/"

instance ToQuery ListUserPolicies where
        toQuery ListUserPolicies'{..}
          = mconcat
              ["Action" =: ("ListUserPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "MaxItems" =: _lupMaxItems, "Marker" =: _lupMarker,
               "UserName" =: _lupUserName]

-- | Contains the response to a successful ListUserPolicies request.
--
-- /See:/ 'listUserPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'luprsMarker'
--
-- * 'luprsIsTruncated'
--
-- * 'luprsStatus'
--
-- * 'luprsPolicyNames'
data ListUserPoliciesResponse = ListUserPoliciesResponse'
    { _luprsMarker      :: !(Maybe Text)
    , _luprsIsTruncated :: !(Maybe Bool)
    , _luprsStatus      :: !Int
    , _luprsPolicyNames :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListUserPoliciesResponse' smart constructor.
listUserPoliciesResponse :: Int -> ListUserPoliciesResponse
listUserPoliciesResponse pStatus_ =
    ListUserPoliciesResponse'
    { _luprsMarker = Nothing
    , _luprsIsTruncated = Nothing
    , _luprsStatus = pStatus_
    , _luprsPolicyNames = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
luprsMarker :: Lens' ListUserPoliciesResponse (Maybe Text)
luprsMarker = lens _luprsMarker (\ s a -> s{_luprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
luprsIsTruncated :: Lens' ListUserPoliciesResponse (Maybe Bool)
luprsIsTruncated = lens _luprsIsTruncated (\ s a -> s{_luprsIsTruncated = a});

-- | FIXME: Undocumented member.
luprsStatus :: Lens' ListUserPoliciesResponse Int
luprsStatus = lens _luprsStatus (\ s a -> s{_luprsStatus = a});

-- | A list of policy names.
luprsPolicyNames :: Lens' ListUserPoliciesResponse [Text]
luprsPolicyNames = lens _luprsPolicyNames (\ s a -> s{_luprsPolicyNames = a});
