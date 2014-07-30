{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.IAM.V2010_05_08.ListInstanceProfiles where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.IAM.V2010_05_08.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'ListInstanceProfiles' request.
listInstanceProfiles :: ListInstanceProfiles
listInstanceProfiles = ListInstanceProfiles
    { _liprMarker = Nothing
    , _liprMaxItems = Nothing
    , _liprPathPrefix = Nothing
    }

data ListInstanceProfiles = ListInstanceProfiles
    { _liprMarker :: Maybe Text
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
    , _liprPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /application_abc/component_xyz/, which would get all instance
      -- profiles whose path starts with /application_abc/component_xyz/.
      -- This parameter is optional. If it is not included, it defaults to
      -- a slash (/), listing all instance profiles.
    } deriving (Generic)

instance ToQuery ListInstanceProfiles where
    toQuery = genericToQuery def

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

data ListInstanceProfilesResponse = ListInstanceProfilesResponse
    { _lipsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more instance profiles to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more instance profiles in the list.
    , _lipsInstanceProfiles :: [InstanceProfile]
      -- ^ A list of instance profiles.
    , _lipsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Generic)

instance FromXML ListInstanceProfilesResponse where
    fromXMLOptions = xmlOptions
