{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetLoginProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves the user name and password-creation date for the specified user.
-- If the user has not been assigned a password, the action returns a 404 ('NoSuchEntity') error.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetLoginProfile.html>
module Network.AWS.IAM.GetLoginProfile
    (
    -- * Request
      GetLoginProfile
    -- ** Request constructor
    , getLoginProfile
    -- ** Request lenses
    , glpUserName

    -- * Response
    , GetLoginProfileResponse
    -- ** Response constructor
    , getLoginProfileResponse
    -- ** Response lenses
    , glprLoginProfile
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype GetLoginProfile = GetLoginProfile
    { _glpUserName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetLoginProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glpUserName' @::@ 'Text'
--
getLoginProfile :: Text -- ^ 'glpUserName'
                -> GetLoginProfile
getLoginProfile p1 = GetLoginProfile
    { _glpUserName = p1
    }

-- | The name of the user whose login profile you want to retrieve.
glpUserName :: Lens' GetLoginProfile Text
glpUserName = lens _glpUserName (\s a -> s { _glpUserName = a })

newtype GetLoginProfileResponse = GetLoginProfileResponse
    { _glprLoginProfile :: LoginProfile
    } deriving (Eq, Read, Show)

-- | 'GetLoginProfileResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glprLoginProfile' @::@ 'LoginProfile'
--
getLoginProfileResponse :: LoginProfile -- ^ 'glprLoginProfile'
                        -> GetLoginProfileResponse
getLoginProfileResponse p1 = GetLoginProfileResponse
    { _glprLoginProfile = p1
    }

-- | The user name and password create date for the user.
glprLoginProfile :: Lens' GetLoginProfileResponse LoginProfile
glprLoginProfile = lens _glprLoginProfile (\s a -> s { _glprLoginProfile = a })

instance ToPath GetLoginProfile where
    toPath = const "/"

instance ToQuery GetLoginProfile where
    toQuery GetLoginProfile{..} = mconcat
        [ "UserName" =? _glpUserName
        ]

instance ToHeaders GetLoginProfile

instance AWSRequest GetLoginProfile where
    type Sv GetLoginProfile = IAM
    type Rs GetLoginProfile = GetLoginProfileResponse

    request  = post "GetLoginProfile"
    response = xmlResponse

instance FromXML GetLoginProfileResponse where
    parseXML = withElement "GetLoginProfileResult" $ \x -> GetLoginProfileResponse
        <$> x .@  "LoginProfile"
