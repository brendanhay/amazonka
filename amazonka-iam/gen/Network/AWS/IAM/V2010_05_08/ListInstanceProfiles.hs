{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListInstanceProfiles
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
module Network.AWS.IAM.V2010_05_08.ListInstanceProfiles
    (
    -- * Request
      ListInstanceProfiles
    -- ** Request constructor
    , mkListInstanceProfilesRequest
    -- ** Request lenses
    , liprPathPrefix
    , liprMarker
    , liprMaxItems

    -- * Response
    , ListInstanceProfilesResponse
    -- ** Response lenses
    , lipsInstanceProfiles
    , lipsIsTruncated
    , lipsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListInstanceProfiles' request.
mkListInstanceProfilesRequest :: ListInstanceProfiles
mkListInstanceProfilesRequest = ListInstanceProfiles
    { _liprPathPrefix = Nothing
    , _liprMarker = Nothing
    , _liprMaxItems = Nothing
    }
{-# INLINE mkListInstanceProfilesRequest #-}

data ListInstanceProfiles = ListInstanceProfiles
    { _liprPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /application_abc/component_xyz/, which would get all instance
      -- profiles whose path starts with /application_abc/component_xyz/.
      -- This parameter is optional. If it is not included, it defaults to
      -- a slash (/), listing all instance profiles.
    , _liprMarker :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , _liprMaxItems :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Show, Generic)

-- | The path prefix for filtering the results. For example:
-- /application_abc/component_xyz/, which would get all instance profiles
-- whose path starts with /application_abc/component_xyz/. This parameter is
-- optional. If it is not included, it defaults to a slash (/), listing all
-- instance profiles.
liprPathPrefix :: Lens' ListInstanceProfiles (Maybe Text)
liprPathPrefix = lens _liprPathPrefix (\s a -> s { _liprPathPrefix = a })
{-# INLINE liprPathPrefix #-}

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
liprMarker :: Lens' ListInstanceProfiles (Maybe Text)
liprMarker = lens _liprMarker (\s a -> s { _liprMarker = a })
{-# INLINE liprMarker #-}

-- | Use this parameter only when paginating results to indicate the maximum
-- number of user names you want in the response. If there are additional user
-- names beyond the maximum you specify, the IsTruncated response element is
-- true. This parameter is optional. If you do not include it, it defaults to
-- 100.
liprMaxItems :: Lens' ListInstanceProfiles (Maybe Integer)
liprMaxItems = lens _liprMaxItems (\s a -> s { _liprMaxItems = a })
{-# INLINE liprMaxItems #-}

instance ToQuery ListInstanceProfiles where
    toQuery = genericQuery def

data ListInstanceProfilesResponse = ListInstanceProfilesResponse
    { _lipsInstanceProfiles :: [InstanceProfile]
      -- ^ A list of instance profiles.
    , _lipsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more instance profiles to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more instance profiles in the list.
    , _lipsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

-- | A list of instance profiles.
lipsInstanceProfiles :: Lens' ListInstanceProfilesResponse ([InstanceProfile])
lipsInstanceProfiles = lens _lipsInstanceProfiles (\s a -> s { _lipsInstanceProfiles = a })
{-# INLINE lipsInstanceProfiles #-}

-- | A flag that indicates whether there are more instance profiles to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more instance profiles in
-- the list.
lipsIsTruncated :: Lens' ListInstanceProfilesResponse (Bool)
lipsIsTruncated = lens _lipsIsTruncated (\s a -> s { _lipsIsTruncated = a })
{-# INLINE lipsIsTruncated #-}

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lipsMarker :: Lens' ListInstanceProfilesResponse (Maybe Text)
lipsMarker = lens _lipsMarker (\s a -> s { _lipsMarker = a })
{-# INLINE lipsMarker #-}

instance FromXML ListInstanceProfilesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListInstanceProfiles where
    type Sv ListInstanceProfiles = IAM
    type Rs ListInstanceProfiles = ListInstanceProfilesResponse

    request = post "ListInstanceProfiles"
    response _ = xmlResponse

instance AWSPager ListInstanceProfiles where
    next rq rs
        | not (_lipsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _liprMarker = _lipsMarker rs
            }
