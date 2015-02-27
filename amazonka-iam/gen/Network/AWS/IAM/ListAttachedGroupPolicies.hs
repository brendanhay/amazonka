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

-- Module      : Network.AWS.IAM.ListAttachedGroupPolicies
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

-- | Lists all managed policies that are attached to the specified group.
--
-- A group can also have inline policies embedded with it. To list the inline
-- policies for a group, use the 'ListGroupPolicies' API. For information about
-- policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/
-- guide.
--
-- You can paginate the results using the 'MaxItems' and 'Marker' parameters. You
-- can use the 'PathPrefix' parameter to limit the list of policies to only those
-- matching the specified path prefix. If there are no policies attached to the
-- specified group (or none that match the specified path prefix), the action
-- returns an empty list.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAttachedGroupPolicies.html>
module Network.AWS.IAM.ListAttachedGroupPolicies
    (
    -- * Request
      ListAttachedGroupPolicies
    -- ** Request constructor
    , listAttachedGroupPolicies
    -- ** Request lenses
    , lagpGroupName
    , lagpMarker
    , lagpMaxItems
    , lagpPathPrefix

    -- * Response
    , ListAttachedGroupPoliciesResponse
    -- ** Response constructor
    , listAttachedGroupPoliciesResponse
    -- ** Response lenses
    , lagprAttachedPolicies
    , lagprIsTruncated
    , lagprMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListAttachedGroupPolicies = ListAttachedGroupPolicies
    { _lagpGroupName  :: Text
    , _lagpMarker     :: Maybe Text
    , _lagpMaxItems   :: Maybe Nat
    , _lagpPathPrefix :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListAttachedGroupPolicies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lagpGroupName' @::@ 'Text'
--
-- * 'lagpMarker' @::@ 'Maybe' 'Text'
--
-- * 'lagpMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lagpPathPrefix' @::@ 'Maybe' 'Text'
--
listAttachedGroupPolicies :: Text -- ^ 'lagpGroupName'
                          -> ListAttachedGroupPolicies
listAttachedGroupPolicies p1 = ListAttachedGroupPolicies
    { _lagpGroupName  = p1
    , _lagpPathPrefix = Nothing
    , _lagpMarker     = Nothing
    , _lagpMaxItems   = Nothing
    }

-- | The name (friendly name, not ARN) of the group to list attached policies for.
lagpGroupName :: Lens' ListAttachedGroupPolicies Text
lagpGroupName = lens _lagpGroupName (\s a -> s { _lagpGroupName = a })

-- | Use this only when paginating results, and only in a subsequent request after
-- you've received a response where the results are truncated. Set it to the
-- value of the 'Marker' element in the response you just received.
lagpMarker :: Lens' ListAttachedGroupPolicies (Maybe Text)
lagpMarker = lens _lagpMarker (\s a -> s { _lagpMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- policies you want in the response. If there are additional policies beyond
-- the maximum you specify, the 'IsTruncated' response element is 'true'. This
-- parameter is optional. If you do not include it, it defaults to 100.
lagpMaxItems :: Lens' ListAttachedGroupPolicies (Maybe Natural)
lagpMaxItems = lens _lagpMaxItems (\s a -> s { _lagpMaxItems = a }) . mapping _Nat

-- | The path prefix for filtering the results. This parameter is optional. If it
-- is not included, it defaults to a slash (/), listing all policies.
lagpPathPrefix :: Lens' ListAttachedGroupPolicies (Maybe Text)
lagpPathPrefix = lens _lagpPathPrefix (\s a -> s { _lagpPathPrefix = a })

data ListAttachedGroupPoliciesResponse = ListAttachedGroupPoliciesResponse
    { _lagprAttachedPolicies :: List "member" AttachedPolicy
    , _lagprIsTruncated      :: Maybe Bool
    , _lagprMarker           :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListAttachedGroupPoliciesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lagprAttachedPolicies' @::@ ['AttachedPolicy']
--
-- * 'lagprIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lagprMarker' @::@ 'Maybe' 'Text'
--
listAttachedGroupPoliciesResponse :: ListAttachedGroupPoliciesResponse
listAttachedGroupPoliciesResponse = ListAttachedGroupPoliciesResponse
    { _lagprAttachedPolicies = mempty
    , _lagprIsTruncated      = Nothing
    , _lagprMarker           = Nothing
    }

-- | A list of the attached policies.
lagprAttachedPolicies :: Lens' ListAttachedGroupPoliciesResponse [AttachedPolicy]
lagprAttachedPolicies =
    lens _lagprAttachedPolicies (\s a -> s { _lagprAttachedPolicies = a })
        . _List

-- | A flag that indicates whether there are more policies to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the 'Marker' request parameter to retrieve more policies in the list.
lagprIsTruncated :: Lens' ListAttachedGroupPoliciesResponse (Maybe Bool)
lagprIsTruncated = lens _lagprIsTruncated (\s a -> s { _lagprIsTruncated = a })

-- | If 'IsTruncated' is 'true', this element is present and contains the value to use
-- for the 'Marker' parameter in a subsequent pagination request.
lagprMarker :: Lens' ListAttachedGroupPoliciesResponse (Maybe Text)
lagprMarker = lens _lagprMarker (\s a -> s { _lagprMarker = a })

instance ToPath ListAttachedGroupPolicies where
    toPath = const "/"

instance ToQuery ListAttachedGroupPolicies where
    toQuery ListAttachedGroupPolicies{..} = mconcat
        [ "GroupName"  =? _lagpGroupName
        , "Marker"     =? _lagpMarker
        , "MaxItems"   =? _lagpMaxItems
        , "PathPrefix" =? _lagpPathPrefix
        ]

instance ToHeaders ListAttachedGroupPolicies

instance AWSRequest ListAttachedGroupPolicies where
    type Sv ListAttachedGroupPolicies = IAM
    type Rs ListAttachedGroupPolicies = ListAttachedGroupPoliciesResponse

    request  = post "ListAttachedGroupPolicies"
    response = xmlResponse

instance FromXML ListAttachedGroupPoliciesResponse where
    parseXML = withElement "ListAttachedGroupPoliciesResult" $ \x -> ListAttachedGroupPoliciesResponse
        <$> x .@? "AttachedPolicies" .!@ mempty
        <*> x .@? "IsTruncated"
        <*> x .@? "Marker"
