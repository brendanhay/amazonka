{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.IAM.ListRoles
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the roles that have the specified path prefix. If there are none, the
-- action returns an empty list. For more information about roles, go to
-- Working with Roles. You can paginate the results using the MaxItems and
-- Marker parameters. The returned policy is URL-encoded according to RFC
-- 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html.
module Network.AWS.IAM.ListRoles
    (
    -- * Request
      ListRoles
    -- ** Request constructor
    , listRoles
    -- ** Request lenses
    , lrMarker
    , lrMaxItems
    , lrPathPrefix

    -- * Response
    , ListRolesResponse
    -- ** Response constructor
    , listRolesResponse
    -- ** Response lenses
    , lrrIsTruncated
    , lrrMarker
    , lrrRoles
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListRoles = ListRoles
    { _lrMarker     :: Maybe Text
    , _lrMaxItems   :: Maybe Natural
    , _lrPathPrefix :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListRoles' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrMarker' @::@ 'Maybe' 'Text'
--
-- * 'lrMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lrPathPrefix' @::@ 'Maybe' 'Text'
--
listRoles :: ListRoles
listRoles = ListRoles
    { _lrPathPrefix = Nothing
    , _lrMarker     = Nothing
    , _lrMaxItems   = Nothing
    }

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lrMarker :: Lens' ListRoles (Maybe Text)
lrMarker = lens _lrMarker (\s a -> s { _lrMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of roles you want in the response. If there are additional roles
-- beyond the maximum you specify, the IsTruncated response element is true.
-- This parameter is optional. If you do not include it, it defaults to 100.
lrMaxItems :: Lens' ListRoles (Maybe Natural)
lrMaxItems = lens _lrMaxItems (\s a -> s { _lrMaxItems = a })

-- | The path prefix for filtering the results. For example, the prefix
-- /application_abc/component_xyz/ gets all roles whose path starts with
-- /application_abc/component_xyz/. This parameter is optional. If it is not
-- included, it defaults to a slash (/), listing all roles.
lrPathPrefix :: Lens' ListRoles (Maybe Text)
lrPathPrefix = lens _lrPathPrefix (\s a -> s { _lrPathPrefix = a })

instance ToQuery ListRoles

instance ToPath ListRoles where
    toPath = const "/"

data ListRolesResponse = ListRolesResponse
    { _lrrIsTruncated :: Maybe Bool
    , _lrrMarker      :: Maybe Text
    , _lrrRoles       :: [Role]
    } deriving (Eq, Show, Generic)

-- | 'ListRolesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lrrMarker' @::@ 'Maybe' 'Text'
--
-- * 'lrrRoles' @::@ ['Role']
--
listRolesResponse :: ListRolesResponse
listRolesResponse = ListRolesResponse
    { _lrrRoles       = mempty
    , _lrrIsTruncated = Nothing
    , _lrrMarker      = Nothing
    }

-- | A flag that indicates whether there are more roles to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more roles in the list.
lrrIsTruncated :: Lens' ListRolesResponse (Maybe Bool)
lrrIsTruncated = lens _lrrIsTruncated (\s a -> s { _lrrIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lrrMarker :: Lens' ListRolesResponse (Maybe Text)
lrrMarker = lens _lrrMarker (\s a -> s { _lrrMarker = a })

-- | A list of roles.
lrrRoles :: Lens' ListRolesResponse [Role]
lrrRoles = lens _lrrRoles (\s a -> s { _lrrRoles = a })

instance AWSRequest ListRoles where
    type Sv ListRoles = IAM
    type Rs ListRoles = ListRolesResponse

    request  = post "ListRoles"
    response = xmlResponse $ \h x -> ListRolesResponse
        <$> x %| "IsTruncated"
        <*> x %| "Marker"
        <*> x %| "Roles"
