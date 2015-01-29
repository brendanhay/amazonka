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

-- Module      : Network.AWS.IAM.ListRolePolicies
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

-- | Lists the names of the policies associated with the specified role. If there
-- are none, the action returns an empty list.
--
-- You can paginate the results using the 'MaxItems' and 'Marker' parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRolePolicies.html>
module Network.AWS.IAM.ListRolePolicies
    (
    -- * Request
      ListRolePolicies
    -- ** Request constructor
    , listRolePolicies
    -- ** Request lenses
    , lrpMarker
    , lrpMaxItems
    , lrpRoleName

    -- * Response
    , ListRolePoliciesResponse
    -- ** Response constructor
    , listRolePoliciesResponse
    -- ** Response lenses
    , lrprIsTruncated
    , lrprMarker
    , lrprPolicyNames
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListRolePolicies = ListRolePolicies
    { _lrpMarker   :: Maybe Text
    , _lrpMaxItems :: Maybe Nat
    , _lrpRoleName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListRolePolicies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrpMarker' @::@ 'Maybe' 'Text'
--
-- * 'lrpMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lrpRoleName' @::@ 'Text'
--
listRolePolicies :: Text -- ^ 'lrpRoleName'
                 -> ListRolePolicies
listRolePolicies p1 = ListRolePolicies
    { _lrpRoleName = p1
    , _lrpMarker   = Nothing
    , _lrpMaxItems = Nothing
    }

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated. Set
-- it to the value of the 'Marker' element in the response you just received.
lrpMarker :: Lens' ListRolePolicies (Maybe Text)
lrpMarker = lens _lrpMarker (\s a -> s { _lrpMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of role policies you want in the response. If there are additional
-- role policies beyond the maximum you specify, the 'IsTruncated' response
-- element is 'true'. This parameter is optional. If you do not include it, it
-- defaults to 100.
lrpMaxItems :: Lens' ListRolePolicies (Maybe Natural)
lrpMaxItems = lens _lrpMaxItems (\s a -> s { _lrpMaxItems = a }) . mapping _Nat

-- | The name of the role to list policies for.
lrpRoleName :: Lens' ListRolePolicies Text
lrpRoleName = lens _lrpRoleName (\s a -> s { _lrpRoleName = a })

data ListRolePoliciesResponse = ListRolePoliciesResponse
    { _lrprIsTruncated :: Maybe Bool
    , _lrprMarker      :: Maybe Text
    , _lrprPolicyNames :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListRolePoliciesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrprIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lrprMarker' @::@ 'Maybe' 'Text'
--
-- * 'lrprPolicyNames' @::@ ['Text']
--
listRolePoliciesResponse :: ListRolePoliciesResponse
listRolePoliciesResponse = ListRolePoliciesResponse
    { _lrprPolicyNames = mempty
    , _lrprIsTruncated = Nothing
    , _lrprMarker      = Nothing
    }

-- | A flag that indicates whether there are more policy names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the 'Marker' request parameter to retrieve more policy names in the list.
lrprIsTruncated :: Lens' ListRolePoliciesResponse (Maybe Bool)
lrprIsTruncated = lens _lrprIsTruncated (\s a -> s { _lrprIsTruncated = a })

-- | If 'IsTruncated' is 'true', this element is present and contains the value to
-- use for the 'Marker' parameter in a subsequent pagination request.
lrprMarker :: Lens' ListRolePoliciesResponse (Maybe Text)
lrprMarker = lens _lrprMarker (\s a -> s { _lrprMarker = a })

-- | A list of policy names.
lrprPolicyNames :: Lens' ListRolePoliciesResponse [Text]
lrprPolicyNames = lens _lrprPolicyNames (\s a -> s { _lrprPolicyNames = a }) . _List

instance ToPath ListRolePolicies where
    toPath = const "/"

instance ToQuery ListRolePolicies where
    toQuery ListRolePolicies{..} = mconcat
        [ "Marker"   =? _lrpMarker
        , "MaxItems" =? _lrpMaxItems
        , "RoleName" =? _lrpRoleName
        ]

instance ToHeaders ListRolePolicies

instance AWSRequest ListRolePolicies where
    type Sv ListRolePolicies = IAM
    type Rs ListRolePolicies = ListRolePoliciesResponse

    request  = post "ListRolePolicies"
    response = xmlResponse

instance FromXML ListRolePoliciesResponse where
    parseXML = withElement "ListRolePoliciesResult" $ \x -> ListRolePoliciesResponse
        <$> x .@? "IsTruncated"
        <*> x .@? "Marker"
        <*> x .@? "PolicyNames" .!@ mempty

instance AWSPager ListRolePolicies where
    page rq rs
        | stop (rs ^. lrprIsTruncated) = Nothing
        | otherwise = Just $ rq
            & lrpMarker .~ rs ^. lrprMarker
