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

-- Module      : Network.AWS.IAM.ListGroupsForUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the groups the specified user belongs to. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupsForUser.html>
module Network.AWS.IAM.ListGroupsForUser
    (
    -- * Request
      ListGroupsForUser
    -- ** Request constructor
    , listGroupsForUser
    -- ** Request lenses
    , lgfuMarker
    , lgfuMaxItems
    , lgfuUserName

    -- * Response
    , ListGroupsForUserResponse
    -- ** Response constructor
    , listGroupsForUserResponse
    -- ** Response lenses
    , lgfurGroups
    , lgfurIsTruncated
    , lgfurMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListGroupsForUser = ListGroupsForUser
    { _lgfuMarker   :: Maybe Text
    , _lgfuMaxItems :: Maybe Nat
    , _lgfuUserName :: Text
    } deriving (Eq, Ord, Show)

-- | 'ListGroupsForUser' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgfuMarker' @::@ 'Maybe' 'Text'
--
-- * 'lgfuMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lgfuUserName' @::@ 'Text'
--
listGroupsForUser :: Text -- ^ 'lgfuUserName'
                  -> ListGroupsForUser
listGroupsForUser p1 = ListGroupsForUser
    { _lgfuUserName = p1
    , _lgfuMarker   = Nothing
    , _lgfuMaxItems = Nothing
    }

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it
-- to the value of the Marker element in the response you just received.
lgfuMarker :: Lens' ListGroupsForUser (Maybe Text)
lgfuMarker = lens _lgfuMarker (\s a -> s { _lgfuMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond
-- the maximum you specify, the IsTruncated response element is true. This
-- parameter is optional. If you do not include it, it defaults to 100.
lgfuMaxItems :: Lens' ListGroupsForUser (Maybe Natural)
lgfuMaxItems = lens _lgfuMaxItems (\s a -> s { _lgfuMaxItems = a }) . mapping _Nat

-- | The name of the user to list groups for.
lgfuUserName :: Lens' ListGroupsForUser Text
lgfuUserName = lens _lgfuUserName (\s a -> s { _lgfuUserName = a })

data ListGroupsForUserResponse = ListGroupsForUserResponse
    { _lgfurGroups      :: List "Groups" Group
    , _lgfurIsTruncated :: Maybe Bool
    , _lgfurMarker      :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListGroupsForUserResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgfurGroups' @::@ ['Group']
--
-- * 'lgfurIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lgfurMarker' @::@ 'Maybe' 'Text'
--
listGroupsForUserResponse :: ListGroupsForUserResponse
listGroupsForUserResponse = ListGroupsForUserResponse
    { _lgfurGroups      = mempty
    , _lgfurIsTruncated = Nothing
    , _lgfurMarker      = Nothing
    }

-- | A list of groups.
lgfurGroups :: Lens' ListGroupsForUserResponse [Group]
lgfurGroups = lens _lgfurGroups (\s a -> s { _lgfurGroups = a }) . _List

-- | A flag that indicates whether there are more groups to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more groups in the list.
lgfurIsTruncated :: Lens' ListGroupsForUserResponse (Maybe Bool)
lgfurIsTruncated = lens _lgfurIsTruncated (\s a -> s { _lgfurIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lgfurMarker :: Lens' ListGroupsForUserResponse (Maybe Text)
lgfurMarker = lens _lgfurMarker (\s a -> s { _lgfurMarker = a })

instance ToPath ListGroupsForUser where
    toPath = const "/"

instance ToQuery ListGroupsForUser where
    toQuery ListGroupsForUser{..} = mconcat
        [ "Marker"   =? _lgfuMarker
        , "MaxItems" =? _lgfuMaxItems
        , "UserName" =? _lgfuUserName
        ]

instance ToHeaders ListGroupsForUser

instance AWSRequest ListGroupsForUser where
    type Sv ListGroupsForUser = IAM
    type Rs ListGroupsForUser = ListGroupsForUserResponse

    request  = post "ListGroupsForUser"
    response = xmlResponse

instance FromXML ListGroupsForUserResponse where
    parseXML = withElement "ListGroupsForUserResult" $ \x -> ListGroupsForUserResponse
        <$> x .@  "Groups"
        <*> x .@? "IsTruncated"
        <*> x .@? "Marker"

instance AWSPager ListGroupsForUser where
    next rq rs
        | stop (rs ^. lgfurIsTruncated) = Nothing
        | otherwise = Just $ rq
            & lgfuMarker .~ rs ^. lgfuMarker


Some kind of operator / class to check the types whether to continue?
