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

-- Module      : Network.AWS.IAM.ListGroupPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the names of the policies associated with the specified group. If
-- there are none, the action returns an empty list.
--
-- You can paginate the results using the 'MaxItems' and 'Marker' parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupPolicies.html>
module Network.AWS.IAM.ListGroupPolicies
    (
    -- * Request
      ListGroupPolicies
    -- ** Request constructor
    , listGroupPolicies
    -- ** Request lenses
    , lgpGroupName
    , lgpMarker
    , lgpMaxItems

    -- * Response
    , ListGroupPoliciesResponse
    -- ** Response constructor
    , listGroupPoliciesResponse
    -- ** Response lenses
    , lgprIsTruncated
    , lgprMarker
    , lgprPolicyNames
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListGroupPolicies = ListGroupPolicies
    { _lgpGroupName :: Text
    , _lgpMarker    :: Maybe Text
    , _lgpMaxItems  :: Maybe Nat
    } deriving (Eq, Ord, Show)

-- | 'ListGroupPolicies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgpGroupName' @::@ 'Text'
--
-- * 'lgpMarker' @::@ 'Maybe' 'Text'
--
-- * 'lgpMaxItems' @::@ 'Maybe' 'Natural'
--
listGroupPolicies :: Text -- ^ 'lgpGroupName'
                  -> ListGroupPolicies
listGroupPolicies p1 = ListGroupPolicies
    { _lgpGroupName = p1
    , _lgpMarker    = Nothing
    , _lgpMaxItems  = Nothing
    }

-- | The name of the group to list policies for.
--
lgpGroupName :: Lens' ListGroupPolicies Text
lgpGroupName = lens _lgpGroupName (\s a -> s { _lgpGroupName = a })

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the 'Marker' element in the response you just received.
--
lgpMarker :: Lens' ListGroupPolicies (Maybe Text)
lgpMarker = lens _lgpMarker (\s a -> s { _lgpMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- policy names you want in the response. If there are additional policy names
-- beyond the maximum you specify, the 'IsTruncated' response element is 'true'.
-- This parameter is optional. If you do not include it, it defaults to 100.
--
lgpMaxItems :: Lens' ListGroupPolicies (Maybe Natural)
lgpMaxItems = lens _lgpMaxItems (\s a -> s { _lgpMaxItems = a }) . mapping _Nat

data ListGroupPoliciesResponse = ListGroupPoliciesResponse
    { _lgprIsTruncated :: Maybe Bool
    , _lgprMarker      :: Maybe Text
    , _lgprPolicyNames :: List "PolicyNames" Text
    } deriving (Eq, Ord, Show)

-- | 'ListGroupPoliciesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgprIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lgprMarker' @::@ 'Maybe' 'Text'
--
-- * 'lgprPolicyNames' @::@ ['Text']
--
listGroupPoliciesResponse :: ListGroupPoliciesResponse
listGroupPoliciesResponse = ListGroupPoliciesResponse
    { _lgprPolicyNames = mempty
    , _lgprIsTruncated = Nothing
    , _lgprMarker      = Nothing
    }

-- | A flag that indicates whether there are more policy names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the 'Marker' request parameter to retrieve more policy names in the list.
--
lgprIsTruncated :: Lens' ListGroupPoliciesResponse (Maybe Bool)
lgprIsTruncated = lens _lgprIsTruncated (\s a -> s { _lgprIsTruncated = a })

-- | If 'IsTruncated' is 'true', this element is present and contains the value to
-- use for the 'Marker' parameter in a subsequent pagination request.
--
lgprMarker :: Lens' ListGroupPoliciesResponse (Maybe Text)
lgprMarker = lens _lgprMarker (\s a -> s { _lgprMarker = a })

-- | A list of policy names.
--
lgprPolicyNames :: Lens' ListGroupPoliciesResponse [Text]
lgprPolicyNames = lens _lgprPolicyNames (\s a -> s { _lgprPolicyNames = a }) . _List

instance ToPath ListGroupPolicies where
    toPath = const "/"

instance ToQuery ListGroupPolicies where
    toQuery ListGroupPolicies{..} = mconcat
        [ "GroupName" =? _lgpGroupName
        , "Marker"    =? _lgpMarker
        , "MaxItems"  =? _lgpMaxItems
        ]

instance ToHeaders ListGroupPolicies

instance AWSRequest ListGroupPolicies where
    type Sv ListGroupPolicies = IAM
    type Rs ListGroupPolicies = ListGroupPoliciesResponse

    request  = post "ListGroupPolicies"
    response = xmlResponse

instance FromXML ListGroupPoliciesResponse where
    parseXML = withElement "ListGroupPoliciesResult" $ \x -> ListGroupPoliciesResponse
        <$> x .@? "IsTruncated"
        <*> x .@? "Marker"
        <*> x .@  "PolicyNames"

instance AWSPager ListGroupPolicies where
    page rq rs
        | stop (rs ^. lgprIsTruncated) = Nothing
        | otherwise = Just $ rq
            & lgpMarker .~ rs ^. lgprMarker
