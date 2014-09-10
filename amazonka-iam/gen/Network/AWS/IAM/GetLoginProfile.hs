{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM
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
module Network.AWS.IAM
    (
    -- * Request
      GetLoginProfile
    -- ** Request constructor
    , mkGetLoginProfile
    -- ** Request lenses
    , glpUserName

    -- * Response
    , GetLoginProfileResponse
    -- ** Response constructor
    , mkGetLoginProfileResponse
    -- ** Response lenses
    , glprLoginProfile
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype GetLoginProfile = GetLoginProfile
    { _glpUserName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetLoginProfile' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
mkGetLoginProfile :: Text -- ^ 'glpUserName'
                  -> GetLoginProfile
mkGetLoginProfile p1 = GetLoginProfile
    { _glpUserName = p1
    }

-- | Name of the user whose login profile you want to retrieve.
glpUserName :: Lens' GetLoginProfile Text
glpUserName = lens _glpUserName (\s a -> s { _glpUserName = a })

instance ToQuery GetLoginProfile where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetLoginProfile
-- action.
newtype GetLoginProfileResponse = GetLoginProfileResponse
    { _glprLoginProfile :: LoginProfile
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetLoginProfileResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoginProfile ::@ @LoginProfile@
--
mkGetLoginProfileResponse :: LoginProfile -- ^ 'glprLoginProfile'
                          -> GetLoginProfileResponse
mkGetLoginProfileResponse p1 = GetLoginProfileResponse
    { _glprLoginProfile = p1
    }

-- | User name and password create date for the user.
glprLoginProfile :: Lens' GetLoginProfileResponse LoginProfile
glprLoginProfile =
    lens _glprLoginProfile (\s a -> s { _glprLoginProfile = a })

instance FromXML GetLoginProfileResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetLoginProfile where
    type Sv GetLoginProfile = IAM
    type Rs GetLoginProfile = GetLoginProfileResponse

    request = post "GetLoginProfile"
    response _ = xmlResponse
