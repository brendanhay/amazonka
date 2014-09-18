{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListInstanceProfiles
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the instance profiles that have the specified path prefix. If there
-- are none, the action returns an empty list. For more information about
-- instance profiles, go to About Instance Profiles. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListInstanceProfiles &MaxItems=100
-- &PathPrefix=/application_abc/ &Version=2010-05-08 &AUTHPARAMS false
-- AIPACIFN4OZXG7EXAMPLE Database /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Database
-- 2012-05-09T16:27:03Z AIPACZLSXM2EYYEXAMPLE Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:27:11Z fd74fa8d-99f3-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.ListInstanceProfiles
    (
    -- * Request
      ListInstanceProfiles
    -- ** Request constructor
    , listInstanceProfiles
    -- ** Request lenses
    , lipPathPrefix
    , lipMarker
    , lipMaxItems

    -- * Response
    , ListInstanceProfilesResponse
    -- ** Response constructor
    , listInstanceProfilesResponse
    -- ** Response lenses
    , liprInstanceProfiles
    , liprIsTruncated
    , liprMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ListInstanceProfiles = ListInstanceProfiles
    { _lipPathPrefix :: Maybe Text
    , _lipMarker :: Maybe Text
    , _lipMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListInstanceProfiles' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PathPrefix ::@ @Maybe Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
listInstanceProfiles :: ListInstanceProfiles
listInstanceProfiles = ListInstanceProfiles
    { _lipPathPrefix = Nothing
    , _lipMarker = Nothing
    , _lipMaxItems = Nothing
    }

-- | The path prefix for filtering the results. For example:
-- /application_abc/component_xyz/, which would get all instance profiles
-- whose path starts with /application_abc/component_xyz/. This parameter is
-- optional. If it is not included, it defaults to a slash (/), listing all
-- instance profiles.
lipPathPrefix :: Lens' ListInstanceProfiles (Maybe Text)
lipPathPrefix = lens _lipPathPrefix (\s a -> s { _lipPathPrefix = a })

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lipMarker :: Lens' ListInstanceProfiles (Maybe Text)
lipMarker = lens _lipMarker (\s a -> s { _lipMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of user names you want in the response. If there are additional user
-- names beyond the maximum you specify, the IsTruncated response element is
-- true. This parameter is optional. If you do not include it, it defaults to
-- 100.
lipMaxItems :: Lens' ListInstanceProfiles (Maybe Integer)
lipMaxItems = lens _lipMaxItems (\s a -> s { _lipMaxItems = a })

instance ToQuery ListInstanceProfiles where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the ListInstanceProfiles
-- action.
data ListInstanceProfilesResponse = ListInstanceProfilesResponse
    { _liprInstanceProfiles :: [InstanceProfile]
    , _liprIsTruncated :: !Bool
    , _liprMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListInstanceProfilesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceProfiles ::@ @[InstanceProfile]@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Marker ::@ @Maybe Text@
--
listInstanceProfilesResponse :: [InstanceProfile] -- ^ 'liprInstanceProfiles'
                               -> Bool -- ^ 'liprIsTruncated'
                               -> ListInstanceProfilesResponse
listInstanceProfilesResponse p1 p2 = ListInstanceProfilesResponse
    { _liprInstanceProfiles = p1
    , _liprIsTruncated = p2
    , _liprMarker = Nothing
    }

-- | A list of instance profiles.
liprInstanceProfiles :: Lens' ListInstanceProfilesResponse [InstanceProfile]
liprInstanceProfiles =
    lens _liprInstanceProfiles (\s a -> s { _liprInstanceProfiles = a })

-- | A flag that indicates whether there are more instance profiles to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more instance profiles in
-- the list.
liprIsTruncated :: Lens' ListInstanceProfilesResponse Bool
liprIsTruncated = lens _liprIsTruncated (\s a -> s { _liprIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
liprMarker :: Lens' ListInstanceProfilesResponse (Maybe Text)
liprMarker = lens _liprMarker (\s a -> s { _liprMarker = a })

instance FromXML ListInstanceProfilesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListInstanceProfiles where
    type Sv ListInstanceProfiles = IAM
    type Rs ListInstanceProfiles = ListInstanceProfilesResponse

    request = post "ListInstanceProfiles"
    response _ = xmlResponse

instance AWSPager ListInstanceProfiles where
    next rq rs
        | not (rs ^. liprIsTruncated) = Nothing
        | otherwise = Just $
            rq & lipMarker .~ rs ^. liprMarker
