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

-- Module      : Network.AWS.IAM.ListUserPolicies
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

-- | Lists the names of the inline policies embedded in the specified user.
--
-- A user can also have managed policies attached to it. To list the managed
-- policies that are attached to a user, use 'ListAttachedUserPolicies'. For more
-- information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in
-- the /Using IAM/ guide.
--
-- You can paginate the results using the 'MaxItems' and 'Marker' parameters. If
-- there are no inline policies embedded with the specified user, the action
-- returns an empty list.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUserPolicies.html>
module Network.AWS.IAM.ListUserPolicies
    (
    -- * Request
      ListUserPolicies
    -- ** Request constructor
    , listUserPolicies
    -- ** Request lenses
    , lupMarker
    , lupMaxItems
    , lupUserName

    -- * Response
    , ListUserPoliciesResponse
    -- ** Response constructor
    , listUserPoliciesResponse
    -- ** Response lenses
    , luprIsTruncated
    , luprMarker
    , luprPolicyNames
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListUserPolicies = ListUserPolicies
    { _lupMarker   :: Maybe Text
    , _lupMaxItems :: Maybe Nat
    , _lupUserName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListUserPolicies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lupMarker' @::@ 'Maybe' 'Text'
--
-- * 'lupMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lupUserName' @::@ 'Text'
--
listUserPolicies :: Text -- ^ 'lupUserName'
                 -> ListUserPolicies
listUserPolicies p1 = ListUserPolicies
    { _lupUserName = p1
    , _lupMarker   = Nothing
    , _lupMaxItems = Nothing
    }

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the 'Marker' element in the response you just received.
lupMarker :: Lens' ListUserPolicies (Maybe Text)
lupMarker = lens _lupMarker (\s a -> s { _lupMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- policy names you want in the response. If there are additional policy names
-- beyond the maximum you specify, the 'IsTruncated' response element is 'true'.
-- This parameter is optional. If you do not include it, it defaults to 100.
lupMaxItems :: Lens' ListUserPolicies (Maybe Natural)
lupMaxItems = lens _lupMaxItems (\s a -> s { _lupMaxItems = a }) . mapping _Nat

-- | The name of the user to list policies for.
lupUserName :: Lens' ListUserPolicies Text
lupUserName = lens _lupUserName (\s a -> s { _lupUserName = a })

data ListUserPoliciesResponse = ListUserPoliciesResponse
    { _luprIsTruncated :: Maybe Bool
    , _luprMarker      :: Maybe Text
    , _luprPolicyNames :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListUserPoliciesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'luprIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'luprMarker' @::@ 'Maybe' 'Text'
--
-- * 'luprPolicyNames' @::@ ['Text']
--
listUserPoliciesResponse :: ListUserPoliciesResponse
listUserPoliciesResponse = ListUserPoliciesResponse
    { _luprPolicyNames = mempty
    , _luprIsTruncated = Nothing
    , _luprMarker      = Nothing
    }

-- | A flag that indicates whether there are more policy names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the 'Marker' request parameter to retrieve more policy names in the list.
luprIsTruncated :: Lens' ListUserPoliciesResponse (Maybe Bool)
luprIsTruncated = lens _luprIsTruncated (\s a -> s { _luprIsTruncated = a })

-- | If 'IsTruncated' is 'true', this element is present and contains the value to
-- use for the 'Marker' parameter in a subsequent pagination request.
luprMarker :: Lens' ListUserPoliciesResponse (Maybe Text)
luprMarker = lens _luprMarker (\s a -> s { _luprMarker = a })

-- | A list of policy names.
luprPolicyNames :: Lens' ListUserPoliciesResponse [Text]
luprPolicyNames = lens _luprPolicyNames (\s a -> s { _luprPolicyNames = a }) . _List

instance ToPath ListUserPolicies where
    toPath = const "/"

instance ToQuery ListUserPolicies where
    toQuery ListUserPolicies{..} = mconcat
        [ "Marker"   =? _lupMarker
        , "MaxItems" =? _lupMaxItems
        , "UserName" =? _lupUserName
        ]

instance ToHeaders ListUserPolicies

instance AWSRequest ListUserPolicies where
    type Sv ListUserPolicies = IAM
    type Rs ListUserPolicies = ListUserPoliciesResponse

    request  = post "ListUserPolicies"
    response = xmlResponse

instance FromXML ListUserPoliciesResponse where
    parseXML = withElement "ListUserPoliciesResult" $ \x -> ListUserPoliciesResponse
        <$> x .@? "IsTruncated"
        <*> x .@? "Marker"
        <*> x .@? "PolicyNames" .!@ mempty

instance AWSPager ListUserPolicies where
    page rq rs
        | stop (rs ^. luprIsTruncated) = Nothing
        | otherwise = Just $ rq
            & lupMarker .~ rs ^. luprMarker
