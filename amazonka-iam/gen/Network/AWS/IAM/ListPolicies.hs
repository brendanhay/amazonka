{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists all the managed policies that are available to your account, including
-- your own customer managed policies and all AWS managed policies.
--
-- You can filter the list of policies that is returned using the optional 'OnlyAttached', 'Scope', and 'PathPrefix' parameters. For example, to list only the customer
-- managed policies in your AWS account, set 'Scope' to 'Local'. To list only AWS
-- managed policies, set 'Scope' to 'AWS'.
--
-- You can paginate the results using the 'MaxItems' and 'Marker' parameters.
--
-- For more information about managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies andInline Policies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListPolicies.html>
module Network.AWS.IAM.ListPolicies
    (
    -- * Request
      ListPolicies
    -- ** Request constructor
    , listPolicies
    -- ** Request lenses
    , lpMarker
    , lpMaxItems
    , lpOnlyAttached
    , lpPathPrefix
    , lpScope

    -- * Response
    , ListPoliciesResponse
    -- ** Response constructor
    , listPoliciesResponse
    -- ** Response lenses
    , lprIsTruncated
    , lprMarker
    , lprPolicies
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListPolicies = ListPolicies
    { _lpMarker       :: Maybe Text
    , _lpMaxItems     :: Maybe Nat
    , _lpOnlyAttached :: Maybe Bool
    , _lpPathPrefix   :: Maybe Text
    , _lpScope        :: Maybe PolicyScopeType
    } deriving (Eq, Read, Show)

-- | 'ListPolicies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpMarker' @::@ 'Maybe' 'Text'
--
-- * 'lpMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lpOnlyAttached' @::@ 'Maybe' 'Bool'
--
-- * 'lpPathPrefix' @::@ 'Maybe' 'Text'
--
-- * 'lpScope' @::@ 'Maybe' 'PolicyScopeType'
--
listPolicies :: ListPolicies
listPolicies = ListPolicies
    { _lpScope        = Nothing
    , _lpOnlyAttached = Nothing
    , _lpPathPrefix   = Nothing
    , _lpMarker       = Nothing
    , _lpMaxItems     = Nothing
    }

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated. Set
-- it to the value of the 'Marker' element in the response you just received.
lpMarker :: Lens' ListPolicies (Maybe Text)
lpMarker = lens _lpMarker (\s a -> s { _lpMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of policies you want in the response. If there are additional policies
-- beyond the maximum you specify, the 'IsTruncated' response element is 'true'.
-- This parameter is optional. If you do not include it, it defaults to 100.
lpMaxItems :: Lens' ListPolicies (Maybe Natural)
lpMaxItems = lens _lpMaxItems (\s a -> s { _lpMaxItems = a }) . mapping _Nat

-- | A flag to filter the results to only the attached policies.
--
-- When 'OnlyAttached' is 'true', the returned list contains only the policies that
-- are attached to a user, group, or role. When 'OnlyAttached' is 'false', or when
-- the parameter is not included, all policies are returned.
lpOnlyAttached :: Lens' ListPolicies (Maybe Bool)
lpOnlyAttached = lens _lpOnlyAttached (\s a -> s { _lpOnlyAttached = a })

-- | The path prefix for filtering the results. This parameter is optional. If it
-- is not included, it defaults to a slash (/), listing all policies.
lpPathPrefix :: Lens' ListPolicies (Maybe Text)
lpPathPrefix = lens _lpPathPrefix (\s a -> s { _lpPathPrefix = a })

-- | The scope to use for filtering the results.
--
-- To list only AWS managed policies, set 'Scope' to 'AWS'. To list only the
-- customer managed policies in your AWS account, set 'Scope' to 'Local'.
--
-- This parameter is optional. If it is not included, or if it is set to 'All',
-- all policies are returned.
lpScope :: Lens' ListPolicies (Maybe PolicyScopeType)
lpScope = lens _lpScope (\s a -> s { _lpScope = a })

data ListPoliciesResponse = ListPoliciesResponse
    { _lprIsTruncated :: Maybe Bool
    , _lprMarker      :: Maybe Text
    , _lprPolicies    :: List "member" Policy
    } deriving (Eq, Read, Show)

-- | 'ListPoliciesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lprMarker' @::@ 'Maybe' 'Text'
--
-- * 'lprPolicies' @::@ ['Policy']
--
listPoliciesResponse :: ListPoliciesResponse
listPoliciesResponse = ListPoliciesResponse
    { _lprPolicies    = mempty
    , _lprIsTruncated = Nothing
    , _lprMarker      = Nothing
    }

-- | A flag that indicates whether there are more policies to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the 'Marker' request parameter to retrieve more policies in the list.
lprIsTruncated :: Lens' ListPoliciesResponse (Maybe Bool)
lprIsTruncated = lens _lprIsTruncated (\s a -> s { _lprIsTruncated = a })

-- | If 'IsTruncated' is 'true', this element is present and contains the value to use
-- for the 'Marker' parameter in a subsequent pagination request.
lprMarker :: Lens' ListPoliciesResponse (Maybe Text)
lprMarker = lens _lprMarker (\s a -> s { _lprMarker = a })

-- | A list of policies.
lprPolicies :: Lens' ListPoliciesResponse [Policy]
lprPolicies = lens _lprPolicies (\s a -> s { _lprPolicies = a }) . _List

instance ToPath ListPolicies where
    toPath = const "/"

instance ToQuery ListPolicies where
    toQuery ListPolicies{..} = mconcat
        [ "Marker"       =? _lpMarker
        , "MaxItems"     =? _lpMaxItems
        , "OnlyAttached" =? _lpOnlyAttached
        , "PathPrefix"   =? _lpPathPrefix
        , "Scope"        =? _lpScope
        ]

instance ToHeaders ListPolicies

instance AWSRequest ListPolicies where
    type Sv ListPolicies = IAM
    type Rs ListPolicies = ListPoliciesResponse

    request  = post "ListPolicies"
    response = xmlResponse

instance FromXML ListPoliciesResponse where
    parseXML = withElement "ListPoliciesResult" $ \x -> ListPoliciesResponse
        <$> x .@? "IsTruncated"
        <*> x .@? "Marker"
        <*> x .@? "Policies" .!@ mempty
