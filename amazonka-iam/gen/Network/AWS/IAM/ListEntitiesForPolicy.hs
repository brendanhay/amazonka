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

-- Module      : Network.AWS.IAM.ListEntitiesForPolicy
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

-- | Lists all users, groups, and roles that the specified managed policy is
-- attached to.
--
-- You can use the optional 'EntityFilter' parameter to limit the results to a
-- particular type of entity (users, groups, or roles). For example, to list
-- only the roles that are attached to the specified policy, set 'EntityFilter' to 'Role'.
--
-- You can paginate the results using the 'MaxItems' and 'Marker' parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListEntitiesForPolicy.html>
module Network.AWS.IAM.ListEntitiesForPolicy
    (
    -- * Request
      ListEntitiesForPolicy
    -- ** Request constructor
    , listEntitiesForPolicy
    -- ** Request lenses
    , lefpEntityFilter
    , lefpMarker
    , lefpMaxItems
    , lefpPathPrefix
    , lefpPolicyArn

    -- * Response
    , ListEntitiesForPolicyResponse
    -- ** Response constructor
    , listEntitiesForPolicyResponse
    -- ** Response lenses
    , lefprIsTruncated
    , lefprMarker
    , lefprPolicyGroups
    , lefprPolicyRoles
    , lefprPolicyUsers
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListEntitiesForPolicy = ListEntitiesForPolicy
    { _lefpEntityFilter :: Maybe EntityType
    , _lefpMarker       :: Maybe Text
    , _lefpMaxItems     :: Maybe Nat
    , _lefpPathPrefix   :: Maybe Text
    , _lefpPolicyArn    :: Text
    } deriving (Eq, Read, Show)

-- | 'ListEntitiesForPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lefpEntityFilter' @::@ 'Maybe' 'EntityType'
--
-- * 'lefpMarker' @::@ 'Maybe' 'Text'
--
-- * 'lefpMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lefpPathPrefix' @::@ 'Maybe' 'Text'
--
-- * 'lefpPolicyArn' @::@ 'Text'
--
listEntitiesForPolicy :: Text -- ^ 'lefpPolicyArn'
                      -> ListEntitiesForPolicy
listEntitiesForPolicy p1 = ListEntitiesForPolicy
    { _lefpPolicyArn    = p1
    , _lefpEntityFilter = Nothing
    , _lefpPathPrefix   = Nothing
    , _lefpMarker       = Nothing
    , _lefpMaxItems     = Nothing
    }

-- | The entity type to use for filtering the results.
--
-- For example, when 'EntityFilter' is 'Role', only the roles that are attached to
-- the specified policy are returned. This parameter is optional. If it is not
-- included, all attached entities (users, groups, and roles) are returned.
lefpEntityFilter :: Lens' ListEntitiesForPolicy (Maybe EntityType)
lefpEntityFilter = lens _lefpEntityFilter (\s a -> s { _lefpEntityFilter = a })

-- | Use this only when paginating results, and only in a subsequent request after
-- you've received a response where the results are truncated. Set it to the
-- value of the 'Marker' element in the response you just received.
lefpMarker :: Lens' ListEntitiesForPolicy (Maybe Text)
lefpMarker = lens _lefpMarker (\s a -> s { _lefpMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- entities you want in the response. If there are additional entities beyond
-- the maximum you specify, the 'IsTruncated' response element is 'true'. This
-- parameter is optional. If you do not include it, it defaults to 100.
lefpMaxItems :: Lens' ListEntitiesForPolicy (Maybe Natural)
lefpMaxItems = lens _lefpMaxItems (\s a -> s { _lefpMaxItems = a }) . mapping _Nat

-- | The path prefix for filtering the results. This parameter is optional. If it
-- is not included, it defaults to a slash (/), listing all entities.
lefpPathPrefix :: Lens' ListEntitiesForPolicy (Maybe Text)
lefpPathPrefix = lens _lefpPathPrefix (\s a -> s { _lefpPathPrefix = a })

lefpPolicyArn :: Lens' ListEntitiesForPolicy Text
lefpPolicyArn = lens _lefpPolicyArn (\s a -> s { _lefpPolicyArn = a })

data ListEntitiesForPolicyResponse = ListEntitiesForPolicyResponse
    { _lefprIsTruncated  :: Maybe Bool
    , _lefprMarker       :: Maybe Text
    , _lefprPolicyGroups :: List "member" PolicyGroup
    , _lefprPolicyRoles  :: List "member" PolicyRole
    , _lefprPolicyUsers  :: List "member" PolicyUser
    } deriving (Eq, Read, Show)

-- | 'ListEntitiesForPolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lefprIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lefprMarker' @::@ 'Maybe' 'Text'
--
-- * 'lefprPolicyGroups' @::@ ['PolicyGroup']
--
-- * 'lefprPolicyRoles' @::@ ['PolicyRole']
--
-- * 'lefprPolicyUsers' @::@ ['PolicyUser']
--
listEntitiesForPolicyResponse :: ListEntitiesForPolicyResponse
listEntitiesForPolicyResponse = ListEntitiesForPolicyResponse
    { _lefprPolicyGroups = mempty
    , _lefprPolicyUsers  = mempty
    , _lefprPolicyRoles  = mempty
    , _lefprIsTruncated  = Nothing
    , _lefprMarker       = Nothing
    }

-- | A flag that indicates whether there are more entities to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the 'Marker' request parameter to retrieve more entities in the list.
lefprIsTruncated :: Lens' ListEntitiesForPolicyResponse (Maybe Bool)
lefprIsTruncated = lens _lefprIsTruncated (\s a -> s { _lefprIsTruncated = a })

-- | If 'IsTruncated' is 'true', this element is present and contains the value to use
-- for the 'Marker' parameter in a subsequent pagination request.
lefprMarker :: Lens' ListEntitiesForPolicyResponse (Maybe Text)
lefprMarker = lens _lefprMarker (\s a -> s { _lefprMarker = a })

-- | A list of groups that the policy is attached to.
lefprPolicyGroups :: Lens' ListEntitiesForPolicyResponse [PolicyGroup]
lefprPolicyGroups =
    lens _lefprPolicyGroups (\s a -> s { _lefprPolicyGroups = a })
        . _List

-- | A list of roles that the policy is attached to.
lefprPolicyRoles :: Lens' ListEntitiesForPolicyResponse [PolicyRole]
lefprPolicyRoles = lens _lefprPolicyRoles (\s a -> s { _lefprPolicyRoles = a }) . _List

-- | A list of users that the policy is attached to.
lefprPolicyUsers :: Lens' ListEntitiesForPolicyResponse [PolicyUser]
lefprPolicyUsers = lens _lefprPolicyUsers (\s a -> s { _lefprPolicyUsers = a }) . _List

instance ToPath ListEntitiesForPolicy where
    toPath = const "/"

instance ToQuery ListEntitiesForPolicy where
    toQuery ListEntitiesForPolicy{..} = mconcat
        [ "EntityFilter" =? _lefpEntityFilter
        , "Marker"       =? _lefpMarker
        , "MaxItems"     =? _lefpMaxItems
        , "PathPrefix"   =? _lefpPathPrefix
        , "PolicyArn"    =? _lefpPolicyArn
        ]

instance ToHeaders ListEntitiesForPolicy

instance AWSRequest ListEntitiesForPolicy where
    type Sv ListEntitiesForPolicy = IAM
    type Rs ListEntitiesForPolicy = ListEntitiesForPolicyResponse

    request  = post "ListEntitiesForPolicy"
    response = xmlResponse

instance FromXML ListEntitiesForPolicyResponse where
    parseXML = withElement "ListEntitiesForPolicyResult" $ \x -> ListEntitiesForPolicyResponse
        <$> x .@? "IsTruncated"
        <*> x .@? "Marker"
        <*> x .@? "PolicyGroups" .!@ mempty
        <*> x .@? "PolicyRoles" .!@ mempty
        <*> x .@? "PolicyUsers" .!@ mempty
