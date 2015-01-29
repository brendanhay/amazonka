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

-- Module      : Network.AWS.IAM.GetGroup
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

-- | Returns a list of users that are in the specified group. You can paginate
-- the results using the 'MaxItems' and 'Marker' parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroup.html>
module Network.AWS.IAM.GetGroup
    (
    -- * Request
      GetGroup
    -- ** Request constructor
    , getGroup
    -- ** Request lenses
    , ggGroupName
    , ggMarker
    , ggMaxItems

    -- * Response
    , GetGroupResponse
    -- ** Response constructor
    , getGroupResponse
    -- ** Response lenses
    , ggrGroup
    , ggrIsTruncated
    , ggrMarker
    , ggrUsers
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data GetGroup = GetGroup
    { _ggGroupName :: Text
    , _ggMarker    :: Maybe Text
    , _ggMaxItems  :: Maybe Nat
    } deriving (Eq, Ord, Read, Show)

-- | 'GetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggGroupName' @::@ 'Text'
--
-- * 'ggMarker' @::@ 'Maybe' 'Text'
--
-- * 'ggMaxItems' @::@ 'Maybe' 'Natural'
--
getGroup :: Text -- ^ 'ggGroupName'
         -> GetGroup
getGroup p1 = GetGroup
    { _ggGroupName = p1
    , _ggMarker    = Nothing
    , _ggMaxItems  = Nothing
    }

-- | The name of the group.
ggGroupName :: Lens' GetGroup Text
ggGroupName = lens _ggGroupName (\s a -> s { _ggGroupName = a })

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the 'Marker' element in the response you just received.
ggMarker :: Lens' GetGroup (Maybe Text)
ggMarker = lens _ggMarker (\s a -> s { _ggMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'. This parameter
-- is optional. If you do not include it, it defaults to 100.
ggMaxItems :: Lens' GetGroup (Maybe Natural)
ggMaxItems = lens _ggMaxItems (\s a -> s { _ggMaxItems = a }) . mapping _Nat

data GetGroupResponse = GetGroupResponse
    { _ggrGroup       :: Group
    , _ggrIsTruncated :: Maybe Bool
    , _ggrMarker      :: Maybe Text
    , _ggrUsers       :: List "member" User
    } deriving (Eq, Read, Show)

-- | 'GetGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggrGroup' @::@ 'Group'
--
-- * 'ggrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'ggrMarker' @::@ 'Maybe' 'Text'
--
-- * 'ggrUsers' @::@ ['User']
--
getGroupResponse :: Group -- ^ 'ggrGroup'
                 -> GetGroupResponse
getGroupResponse p1 = GetGroupResponse
    { _ggrGroup       = p1
    , _ggrUsers       = mempty
    , _ggrIsTruncated = Nothing
    , _ggrMarker      = Nothing
    }

-- | Information about the group.
ggrGroup :: Lens' GetGroupResponse Group
ggrGroup = lens _ggrGroup (\s a -> s { _ggrGroup = a })

-- | A flag that indicates whether there are more user names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the 'Marker' request parameter to retrieve more user names in the list.
ggrIsTruncated :: Lens' GetGroupResponse (Maybe Bool)
ggrIsTruncated = lens _ggrIsTruncated (\s a -> s { _ggrIsTruncated = a })

-- | If IsTruncated is 'true', then this element is present and contains the value
-- to use for the 'Marker' parameter in a subsequent pagination request.
ggrMarker :: Lens' GetGroupResponse (Maybe Text)
ggrMarker = lens _ggrMarker (\s a -> s { _ggrMarker = a })

-- | A list of users in the group.
ggrUsers :: Lens' GetGroupResponse [User]
ggrUsers = lens _ggrUsers (\s a -> s { _ggrUsers = a }) . _List

instance ToPath GetGroup where
    toPath = const "/"

instance ToQuery GetGroup where
    toQuery GetGroup{..} = mconcat
        [ "GroupName" =? _ggGroupName
        , "Marker"    =? _ggMarker
        , "MaxItems"  =? _ggMaxItems
        ]

instance ToHeaders GetGroup

instance AWSRequest GetGroup where
    type Sv GetGroup = IAM
    type Rs GetGroup = GetGroupResponse

    request  = post "GetGroup"
    response = xmlResponse

instance FromXML GetGroupResponse where
    parseXML = withElement "GetGroupResult" $ \x -> GetGroupResponse
        <$> x .@  "Group"
        <*> x .@? "IsTruncated"
        <*> x .@? "Marker"
        <*> x .@? "Users" .!@ mempty

instance AWSPager GetGroup where
    page rq rs
        | stop (rs ^. ggrIsTruncated) = Nothing
        | otherwise = Just $ rq
            & ggMarker .~ rs ^. ggrMarker
