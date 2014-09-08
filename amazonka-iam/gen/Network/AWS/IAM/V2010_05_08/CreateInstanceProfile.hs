{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.IAM.V2010_05_08.CreateInstanceProfile
    (
    -- * Request
      CreateInstanceProfile
    -- ** Request constructor
    , mkCreateInstanceProfile
    -- ** Request lenses
    , cipInstanceProfileName
    , cipPath

    -- * Response
    , CreateInstanceProfileResponse
    -- ** Response lenses
    , ciprInstanceProfile
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
data CreateInstanceProfile = CreateInstanceProfile
    { _cipInstanceProfileName :: Text
    , _cipPath :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInstanceProfile' request.
mkCreateInstanceProfile :: Text -- ^ 'cipInstanceProfileName'
                        -> CreateInstanceProfile
mkCreateInstanceProfile p1 = CreateInstanceProfile
    { _cipInstanceProfileName = p1
    , _cipPath = Nothing
    }

-- | Name of the instance profile to create.
cipInstanceProfileName :: Lens' CreateInstanceProfile Text
cipInstanceProfileName =
    lens _cipInstanceProfileName (\s a -> s { _cipInstanceProfileName = a })

-- | The path to the instance profile. For more information about paths, see
-- Identifiers for IAM Entities in the Using IAM guide. This parameter is
-- optional. If it is not included, it defaults to a slash (/).
cipPath :: Lens' CreateInstanceProfile (Maybe Text)
cipPath = lens _cipPath (\s a -> s { _cipPath = a })

instance ToQuery CreateInstanceProfile where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the CreateInstanceProfile
-- action.
newtype CreateInstanceProfileResponse = CreateInstanceProfileResponse
    { _ciprInstanceProfile :: InstanceProfile
    } deriving (Show, Generic)

-- | Information about the instance profile.
ciprInstanceProfile :: Lens' CreateInstanceProfileResponse InstanceProfile
ciprInstanceProfile =
    lens _ciprInstanceProfile (\s a -> s { _ciprInstanceProfile = a })

instance FromXML CreateInstanceProfileResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateInstanceProfile where
    type Sv CreateInstanceProfile = IAM
    type Rs CreateInstanceProfile = CreateInstanceProfileResponse

    request = post "CreateInstanceProfile"
    response _ = xmlResponse
