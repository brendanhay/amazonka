{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListInstanceProfilesForRole
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
-- https://iam.amazonaws.com/ ?Action=ListInstanceProfilesForRole
-- &MaxItems=100 &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS false
-- AIPACZLS2EYYXMEXAMPLE /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:35Z AROACVSVTSZYK3EXAMPLE Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:27:11Z 6a8c3992-99f4-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.ListInstanceProfilesForRole
    (
    -- * Request
      ListInstanceProfilesForRole
    -- ** Request constructor
    , mkListInstanceProfilesForRole
    -- ** Request lenses
    , lipfrRoleName
    , lipfrMarker
    , lipfrMaxItems

    -- * Response
    , ListInstanceProfilesForRoleResponse
    -- ** Response lenses
    , lipfrrsInstanceProfiles
    , lipfrrsIsTruncated
    , lipfrrsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
data ListInstanceProfilesForRole = ListInstanceProfilesForRole
    { _lipfrRoleName :: Text
    , _lipfrMarker :: Maybe Text
    , _lipfrMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListInstanceProfilesForRole' request.
mkListInstanceProfilesForRole :: Text -- ^ 'lipfrRoleName'
                              -> ListInstanceProfilesForRole
mkListInstanceProfilesForRole p1 = ListInstanceProfilesForRole
    { _lipfrRoleName = p1
    , _lipfrMarker = Nothing
    , _lipfrMaxItems = Nothing
    }

-- | The name of the role to list instance profiles for.
lipfrRoleName :: Lens' ListInstanceProfilesForRole Text
lipfrRoleName = lens _lipfrRoleName (\s a -> s { _lipfrRoleName = a })

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lipfrMarker :: Lens' ListInstanceProfilesForRole (Maybe Text)
lipfrMarker = lens _lipfrMarker (\s a -> s { _lipfrMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of user names you want in the response. If there are additional user
-- names beyond the maximum you specify, the IsTruncated response element is
-- true. This parameter is optional. If you do not include it, it defaults to
-- 100.
lipfrMaxItems :: Lens' ListInstanceProfilesForRole (Maybe Integer)
lipfrMaxItems = lens _lipfrMaxItems (\s a -> s { _lipfrMaxItems = a })

instance ToQuery ListInstanceProfilesForRole where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- ListInstanceProfilesForRole action.
data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse
    { _lipfrrsInstanceProfiles :: [InstanceProfile]
    , _lipfrrsIsTruncated :: Bool
    , _lipfrrsMarker :: Maybe Text
    } deriving (Show, Generic)

-- | A list of instance profiles.
lipfrrsInstanceProfiles :: Lens' ListInstanceProfilesForRoleResponse [InstanceProfile]
lipfrrsInstanceProfiles =
    lens _lipfrrsInstanceProfiles
         (\s a -> s { _lipfrrsInstanceProfiles = a })

-- | A flag that indicates whether there are more instance profiles to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more instance profiles in
-- the list.
lipfrrsIsTruncated :: Lens' ListInstanceProfilesForRoleResponse Bool
lipfrrsIsTruncated =
    lens _lipfrrsIsTruncated (\s a -> s { _lipfrrsIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lipfrrsMarker :: Lens' ListInstanceProfilesForRoleResponse (Maybe Text)
lipfrrsMarker = lens _lipfrrsMarker (\s a -> s { _lipfrrsMarker = a })

instance FromXML ListInstanceProfilesForRoleResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListInstanceProfilesForRole where
    type Sv ListInstanceProfilesForRole = IAM
    type Rs ListInstanceProfilesForRole = ListInstanceProfilesForRoleResponse

    request = post "ListInstanceProfilesForRole"
    response _ = xmlResponse

instance AWSPager ListInstanceProfilesForRole where
    next rq rs
        | not (rs ^. lipfrrsIsTruncated) = Nothing
        | otherwise = Just (rq & lipfrMarker .~ rs ^. lipfrrsMarker)
