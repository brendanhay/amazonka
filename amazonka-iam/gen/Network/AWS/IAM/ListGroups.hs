{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the groups that have the specified path prefix. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroups.html>
module Network.AWS.IAM.ListGroups
    (
    -- * Request
      ListGroups
    -- ** Request constructor
    , listGroups
    -- ** Request lenses
    , lgMarker
    , lgMaxItems
    , lgPathPrefix

    -- * Response
    , ListGroupsResponse
    -- ** Response constructor
    , listGroupsResponse
    -- ** Response lenses
    , lgrGroups
    , lgrIsTruncated
    , lgrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListGroups = ListGroups
    { _lgMarker     :: Maybe Text
    , _lgMaxItems   :: Maybe Nat
    , _lgPathPrefix :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgMarker' @::@ 'Maybe' 'Text'
--
-- * 'lgMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lgPathPrefix' @::@ 'Maybe' 'Text'
--
listGroups :: ListGroups
listGroups = ListGroups
    { _lgPathPrefix = Nothing
    , _lgMarker     = Nothing
    , _lgMaxItems   = Nothing
    }

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it
-- to the value of the Marker element in the response you just received.
lgMarker :: Lens' ListGroups (Maybe Text)
lgMarker = lens _lgMarker (\s a -> s { _lgMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond
-- the maximum you specify, the IsTruncated response element is true. This
-- parameter is optional. If you do not include it, it defaults to 100.
lgMaxItems :: Lens' ListGroups (Maybe Natural)
lgMaxItems = lens _lgMaxItems (\s a -> s { _lgMaxItems = a })
    . mapping _Nat

-- | The path prefix for filtering the results. For example, the prefix
-- /division_abc/subdivision_xyz/ gets all groups whose path starts with
-- /division_abc/subdivision_xyz/. This parameter is optional. If it is not
-- included, it defaults to a slash (/), listing all groups.
lgPathPrefix :: Lens' ListGroups (Maybe Text)
lgPathPrefix = lens _lgPathPrefix (\s a -> s { _lgPathPrefix = a })

data ListGroupsResponse = ListGroupsResponse
    { _lgrGroups      :: [Group]
    , _lgrIsTruncated :: Maybe Bool
    , _lgrMarker      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgrGroups' @::@ ['Group']
--
-- * 'lgrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lgrMarker' @::@ 'Maybe' 'Text'
--
listGroupsResponse :: ListGroupsResponse
listGroupsResponse = ListGroupsResponse
    { _lgrGroups      = mempty
    , _lgrIsTruncated = Nothing
    , _lgrMarker      = Nothing
    }

-- | A list of groups.
lgrGroups :: Lens' ListGroupsResponse [Group]
lgrGroups = lens _lgrGroups (\s a -> s { _lgrGroups = a })

-- | A flag that indicates whether there are more groups to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more groups in the list.
lgrIsTruncated :: Lens' ListGroupsResponse (Maybe Bool)
lgrIsTruncated = lens _lgrIsTruncated (\s a -> s { _lgrIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lgrMarker :: Lens' ListGroupsResponse (Maybe Text)
lgrMarker = lens _lgrMarker (\s a -> s { _lgrMarker = a })

instance AWSRequest ListGroups where
    type Sv ListGroups = IAM
    type Rs ListGroups = ListGroupsResponse

    request  = post "ListGroups"
    response = xmlResponse

instance FromXML ListGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListGroupsResponse"

instance ToPath ListGroups where
    toPath = const "/"

instance ToHeaders ListGroups

instance ToQuery ListGroups
