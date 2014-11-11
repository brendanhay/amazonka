{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.ListInstanceProfilesForRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the instance profiles that have the specified associated role. If
-- there are none, the action returns an empty list. For more information
-- about instance profiles, go to About Instance Profiles. You can paginate
-- the results using the MaxItems and Marker parameters.
module Network.AWS.IAM.ListInstanceProfilesForRole
    (
    -- * Request
      ListInstanceProfilesForRole
    -- ** Request constructor
    , listInstanceProfilesForRole
    -- ** Request lenses
    , lipfrMarker
    , lipfrMaxItems
    , lipfrRoleName

    -- * Response
    , ListInstanceProfilesForRoleResponse
    -- ** Response constructor
    , listInstanceProfilesForRoleResponse
    -- ** Response lenses
    , lipfrrInstanceProfiles
    , lipfrrIsTruncated
    , lipfrrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

data ListInstanceProfilesForRole = ListInstanceProfilesForRole
    { _lipfrMarker   :: Maybe Text
    , _lipfrMaxItems :: Maybe Int
    , _lipfrRoleName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListInstanceProfilesForRole' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipfrMarker' @::@ 'Maybe' 'Text'
--
-- * 'lipfrMaxItems' @::@ 'Maybe' 'Int'
--
-- * 'lipfrRoleName' @::@ 'Text'
--
listInstanceProfilesForRole :: Text -- ^ 'lipfrRoleName'
                            -> ListInstanceProfilesForRole
listInstanceProfilesForRole p1 = ListInstanceProfilesForRole
    { _lipfrRoleName = p1
    , _lipfrMarker   = Nothing
    , _lipfrMaxItems = Nothing
    }

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lipfrMarker :: Lens' ListInstanceProfilesForRole (Maybe Text)
lipfrMarker = lens _lipfrMarker (\s a -> s { _lipfrMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of instance profiles you want in the response. If there are
-- additional instance profiles beyond the maximum you specify, the
-- IsTruncated response element is true. This parameter is optional. If you
-- do not include it, it defaults to 100.
lipfrMaxItems :: Lens' ListInstanceProfilesForRole (Maybe Int)
lipfrMaxItems = lens _lipfrMaxItems (\s a -> s { _lipfrMaxItems = a })

-- | The name of the role to list instance profiles for.
lipfrRoleName :: Lens' ListInstanceProfilesForRole Text
lipfrRoleName = lens _lipfrRoleName (\s a -> s { _lipfrRoleName = a })
instance ToQuery ListInstanceProfilesForRole

instance ToPath ListInstanceProfilesForRole where
    toPath = const "/"

data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse
    { _lipfrrInstanceProfiles :: [InstanceProfile]
    , _lipfrrIsTruncated      :: Maybe Bool
    , _lipfrrMarker           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListInstanceProfilesForRoleResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipfrrInstanceProfiles' @::@ ['InstanceProfile']
--
-- * 'lipfrrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lipfrrMarker' @::@ 'Maybe' 'Text'
--
listInstanceProfilesForRoleResponse :: ListInstanceProfilesForRoleResponse
listInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse
    { _lipfrrInstanceProfiles = mempty
    , _lipfrrIsTruncated      = Nothing
    , _lipfrrMarker           = Nothing
    }

-- | A list of instance profiles.
lipfrrInstanceProfiles :: Lens' ListInstanceProfilesForRoleResponse [InstanceProfile]
lipfrrInstanceProfiles =
    lens _lipfrrInstanceProfiles (\s a -> s { _lipfrrInstanceProfiles = a })

-- | A flag that indicates whether there are more instance profiles to list.
-- If your results were truncated, you can make a subsequent pagination
-- request using the Marker request parameter to retrieve more instance
-- profiles in the list.
lipfrrIsTruncated :: Lens' ListInstanceProfilesForRoleResponse (Maybe Bool)
lipfrrIsTruncated =
    lens _lipfrrIsTruncated (\s a -> s { _lipfrrIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lipfrrMarker :: Lens' ListInstanceProfilesForRoleResponse (Maybe Text)
lipfrrMarker = lens _lipfrrMarker (\s a -> s { _lipfrrMarker = a })
instance FromXML ListInstanceProfilesForRoleResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListInstanceProfilesForRoleResponse"

instance AWSRequest ListInstanceProfilesForRole where
    type Sv ListInstanceProfilesForRole = IAM
    type Rs ListInstanceProfilesForRole = ListInstanceProfilesForRoleResponse

    request  = post "ListInstanceProfilesForRole"
    response = xmlResponse $ \h x -> ListInstanceProfilesForRoleResponse
        <$> x %| "InstanceProfiles"
        <*> x %| "IsTruncated"
        <*> x %| "Marker"
