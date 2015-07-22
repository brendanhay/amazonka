{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists all the managed policies that are available to your account,
-- including your own customer managed policies and all AWS managed
-- policies.
--
-- You can filter the list of policies that is returned using the optional
-- @OnlyAttached@, @Scope@, and @PathPrefix@ parameters. For example, to
-- list only the customer managed policies in your AWS account, set @Scope@
-- to @Local@. To list only AWS managed policies, set @Scope@ to @AWS@.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- For more information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListPolicies.html>
module Network.AWS.IAM.ListPolicies
    (
    -- * Request
      ListPolicies
    -- ** Request constructor
    , listPolicies
    -- ** Request lenses
    , lprqPathPrefix
    , lprqOnlyAttached
    , lprqScope
    , lprqMaxItems
    , lprqMarker

    -- * Response
    , ListPoliciesResponse
    -- ** Response constructor
    , listPoliciesResponse
    -- ** Response lenses
    , lprsMarker
    , lprsIsTruncated
    , lprsPolicies
    , lprsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprqPathPrefix'
--
-- * 'lprqOnlyAttached'
--
-- * 'lprqScope'
--
-- * 'lprqMaxItems'
--
-- * 'lprqMarker'
data ListPolicies = ListPolicies'
    { _lprqPathPrefix   :: !(Maybe Text)
    , _lprqOnlyAttached :: !(Maybe Bool)
    , _lprqScope        :: !(Maybe PolicyScopeType)
    , _lprqMaxItems     :: !(Maybe Nat)
    , _lprqMarker       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPolicies' smart constructor.
listPolicies :: ListPolicies
listPolicies =
    ListPolicies'
    { _lprqPathPrefix = Nothing
    , _lprqOnlyAttached = Nothing
    , _lprqScope = Nothing
    , _lprqMaxItems = Nothing
    , _lprqMarker = Nothing
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
lprqPathPrefix :: Lens' ListPolicies (Maybe Text)
lprqPathPrefix = lens _lprqPathPrefix (\ s a -> s{_lprqPathPrefix = a});

-- | A flag to filter the results to only the attached policies.
--
-- When @OnlyAttached@ is @true@, the returned list contains only the
-- policies that are attached to a user, group, or role. When
-- @OnlyAttached@ is @false@, or when the parameter is not included, all
-- policies are returned.
lprqOnlyAttached :: Lens' ListPolicies (Maybe Bool)
lprqOnlyAttached = lens _lprqOnlyAttached (\ s a -> s{_lprqOnlyAttached = a});

-- | The scope to use for filtering the results.
--
-- To list only AWS managed policies, set @Scope@ to @AWS@. To list only
-- the customer managed policies in your AWS account, set @Scope@ to
-- @Local@.
--
-- This parameter is optional. If it is not included, or if it is set to
-- @All@, all policies are returned.
lprqScope :: Lens' ListPolicies (Maybe PolicyScopeType)
lprqScope = lens _lprqScope (\ s a -> s{_lprqScope = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lprqMaxItems :: Lens' ListPolicies (Maybe Natural)
lprqMaxItems = lens _lprqMaxItems (\ s a -> s{_lprqMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lprqMarker :: Lens' ListPolicies (Maybe Text)
lprqMarker = lens _lprqMarker (\ s a -> s{_lprqMarker = a});

instance AWSPager ListPolicies where
        page rq rs
          | stop (rs ^. lprsIsTruncated) = Nothing
          | isNothing (rs ^. lprsMarker) = Nothing
          | otherwise =
            Just $ rq & lprqMarker .~ rs ^. lprsMarker

instance AWSRequest ListPolicies where
        type Sv ListPolicies = IAM
        type Rs ListPolicies = ListPoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListPoliciesResult"
              (\ s h x ->
                 ListPoliciesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (x .@? "Policies" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders ListPolicies where
        toHeaders = const mempty

instance ToPath ListPolicies where
        toPath = const "/"

instance ToQuery ListPolicies where
        toQuery ListPolicies'{..}
          = mconcat
              ["Action" =: ("ListPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lprqPathPrefix,
               "OnlyAttached" =: _lprqOnlyAttached,
               "Scope" =: _lprqScope, "MaxItems" =: _lprqMaxItems,
               "Marker" =: _lprqMarker]

-- | Contains the response to a successful ListPolicies request.
--
-- /See:/ 'listPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprsMarker'
--
-- * 'lprsIsTruncated'
--
-- * 'lprsPolicies'
--
-- * 'lprsStatus'
data ListPoliciesResponse = ListPoliciesResponse'
    { _lprsMarker      :: !(Maybe Text)
    , _lprsIsTruncated :: !(Maybe Bool)
    , _lprsPolicies    :: !(Maybe [Policy])
    , _lprsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPoliciesResponse' smart constructor.
listPoliciesResponse :: Int -> ListPoliciesResponse
listPoliciesResponse pStatus_ =
    ListPoliciesResponse'
    { _lprsMarker = Nothing
    , _lprsIsTruncated = Nothing
    , _lprsPolicies = Nothing
    , _lprsStatus = pStatus_
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lprsMarker :: Lens' ListPoliciesResponse (Maybe Text)
lprsMarker = lens _lprsMarker (\ s a -> s{_lprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lprsIsTruncated :: Lens' ListPoliciesResponse (Maybe Bool)
lprsIsTruncated = lens _lprsIsTruncated (\ s a -> s{_lprsIsTruncated = a});

-- | A list of policies.
lprsPolicies :: Lens' ListPoliciesResponse [Policy]
lprsPolicies = lens _lprsPolicies (\ s a -> s{_lprsPolicies = a}) . _Default;

-- | FIXME: Undocumented member.
lprsStatus :: Lens' ListPoliciesResponse Int
lprsStatus = lens _lprsStatus (\ s a -> s{_lprsStatus = a});
