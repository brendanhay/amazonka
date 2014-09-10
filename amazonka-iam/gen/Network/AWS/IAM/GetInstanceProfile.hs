{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetInstanceProfile
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
module Network.AWS.IAM
    (
    -- * Request
      GetInstanceProfile
    -- ** Request constructor
    , mkGetInstanceProfile
    -- ** Request lenses
    , gipInstanceProfileName

    -- * Response
    , GetInstanceProfileResponse
    -- ** Response constructor
    , mkGetInstanceProfileResponse
    -- ** Response lenses
    , giprInstanceProfile
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype GetInstanceProfile = GetInstanceProfile
    { _gipInstanceProfileName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetInstanceProfile' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceProfileName ::@ @Text@
--
mkGetInstanceProfile :: Text -- ^ 'gipInstanceProfileName'
                     -> GetInstanceProfile
mkGetInstanceProfile p1 = GetInstanceProfile
    { _gipInstanceProfileName = p1
    }

-- | Name of the instance profile to get information about.
gipInstanceProfileName :: Lens' GetInstanceProfile Text
gipInstanceProfileName =
    lens _gipInstanceProfileName (\s a -> s { _gipInstanceProfileName = a })

instance ToQuery GetInstanceProfile where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetInstanceProfile
-- action.
newtype GetInstanceProfileResponse = GetInstanceProfileResponse
    { _giprInstanceProfile :: InstanceProfile
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetInstanceProfileResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceProfile ::@ @InstanceProfile@
--
mkGetInstanceProfileResponse :: InstanceProfile -- ^ 'giprInstanceProfile'
                             -> GetInstanceProfileResponse
mkGetInstanceProfileResponse p1 = GetInstanceProfileResponse
    { _giprInstanceProfile = p1
    }

-- | Information about the instance profile.
giprInstanceProfile :: Lens' GetInstanceProfileResponse InstanceProfile
giprInstanceProfile =
    lens _giprInstanceProfile (\s a -> s { _giprInstanceProfile = a })

instance FromXML GetInstanceProfileResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetInstanceProfile where
    type Sv GetInstanceProfile = IAM
    type Rs GetInstanceProfile = GetInstanceProfileResponse

    request = post "GetInstanceProfile"
    response _ = xmlResponse
