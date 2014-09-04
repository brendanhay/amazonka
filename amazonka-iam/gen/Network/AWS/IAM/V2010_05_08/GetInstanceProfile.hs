{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified instance profile, including the
-- instance profile's path, GUID, ARN, and role. For more information about
-- instance profiles, go to About Instance Profiles. For more information
-- about ARNs, go to ARNs. https://iam.amazonaws.com/
-- ?Action=GetInstanceProfile &InstanceProfileName=Webserver
-- &Version=2010-05-08 &AUTHPARAMS AIPAD5ARO2C5EXAMPLE3G
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:35Z AROACVYKSVTSZFEXAMPLE Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:11:10Z 37289fda-99f2-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.GetInstanceProfile
    (
    -- * Request
      GetInstanceProfile
    -- ** Request constructor
    , getInstanceProfile
    -- ** Request lenses
    , giprInstanceProfileName

    -- * Response
    , GetInstanceProfileResponse
    -- ** Response lenses
    , gipsInstanceProfile
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetInstanceProfile' request.
getInstanceProfile :: Text -- ^ 'giprInstanceProfileName'
                   -> GetInstanceProfile
getInstanceProfile p1 = GetInstanceProfile
    { _giprInstanceProfileName = p1
    }
{-# INLINE getInstanceProfile #-}

data GetInstanceProfile = GetInstanceProfile
    { _giprInstanceProfileName :: Text
      -- ^ Name of the instance profile to get information about.
    } deriving (Show, Generic)

-- | Name of the instance profile to get information about.
giprInstanceProfileName :: Lens' GetInstanceProfile (Text)
giprInstanceProfileName f x =
    f (_giprInstanceProfileName x)
        <&> \y -> x { _giprInstanceProfileName = y }
{-# INLINE giprInstanceProfileName #-}

instance ToQuery GetInstanceProfile where
    toQuery = genericQuery def

data GetInstanceProfileResponse = GetInstanceProfileResponse
    { _gipsInstanceProfile :: InstanceProfile
      -- ^ Information about the instance profile.
    } deriving (Show, Generic)

-- | Information about the instance profile.
gipsInstanceProfile :: Lens' GetInstanceProfileResponse (InstanceProfile)
gipsInstanceProfile f x =
    f (_gipsInstanceProfile x)
        <&> \y -> x { _gipsInstanceProfile = y }
{-# INLINE gipsInstanceProfile #-}

instance FromXML GetInstanceProfileResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetInstanceProfile where
    type Sv GetInstanceProfile = IAM
    type Rs GetInstanceProfile = GetInstanceProfileResponse

    request = post "GetInstanceProfile"
    response _ = xmlResponse
