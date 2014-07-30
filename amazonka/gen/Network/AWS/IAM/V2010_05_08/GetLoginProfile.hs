{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetLoginProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the user name and password-creation date for the specified user.
-- If the user has not been assigned a password, the action returns a 404
-- (NoSuchEntity) error. https://iam.amazonaws.com/ ?Action=GetLoginProfile
-- &UserName=Bob &AUTHPARAMS Bob 2011-09-19T23:00:56Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.GetLoginProfile where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data GetLoginProfile = GetLoginProfile
    { _glprUserName :: Text
      -- ^ Name of the user whose login profile you want to retrieve.
    } deriving (Generic)

instance ToQuery GetLoginProfile where
    toQuery = genericToQuery def

instance AWSRequest GetLoginProfile where
    type Sv GetLoginProfile = IAM
    type Rs GetLoginProfile = GetLoginProfileResponse

    request = post "GetLoginProfile"
    response _ = xmlResponse

data GetLoginProfileResponse = GetLoginProfileResponse
    { _glpsLoginProfile :: LoginProfile
      -- ^ User name and password create date for the user.
    } deriving (Generic)

instance FromXML GetLoginProfileResponse where
    fromXMLOptions = xmlOptions
