{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.IAM.V2010_05_08.CreateInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new instance profile. For information about instance profiles, go
-- to About Instance Profiles. For information about the number of instance
-- profiles you can create, see Limitations on IAM Entities in the Using IAM
-- guide. https://iam.amazonaws.com/ ?Action=CreateInstanceProfile
-- &InstanceProfileName=Webserver &Path=/application_abc/component_xyz/
-- &Version=2010-05-08 &AUTHPARAMS AIPAD5ARO2C5EXAMPLE3G Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:11:10.222Z 974142ee-99f1-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.CreateInstanceProfile where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateInstanceProfile' request.
createInstanceProfile :: Text -- ^ '_ciprInstanceProfileName'
                      -> CreateInstanceProfile
createInstanceProfile p1 = CreateInstanceProfile
    { _ciprInstanceProfileName = p1
    , _ciprPath = Nothing
    }

data CreateInstanceProfile = CreateInstanceProfile
    { _ciprInstanceProfileName :: Text
      -- ^ Name of the instance profile to create.
    , _ciprPath :: Maybe Text
      -- ^ The path to the instance profile. For more information about
      -- paths, see Identifiers for IAM Entities in the Using IAM guide.
      -- This parameter is optional. If it is not included, it defaults to
      -- a slash (/).
    } deriving (Generic)

instance ToQuery CreateInstanceProfile where
    toQuery = genericToQuery def

instance AWSRequest CreateInstanceProfile where
    type Sv CreateInstanceProfile = IAM
    type Rs CreateInstanceProfile = CreateInstanceProfileResponse

    request = post "CreateInstanceProfile"
    response _ = xmlResponse

data CreateInstanceProfileResponse = CreateInstanceProfileResponse
    { _cipsInstanceProfile :: InstanceProfile
      -- ^ Information about the instance profile.
    } deriving (Generic)

instance FromXML CreateInstanceProfileResponse where
    fromXMLOptions = xmlOptions
