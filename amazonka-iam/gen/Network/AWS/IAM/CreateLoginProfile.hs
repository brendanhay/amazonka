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

-- Module      : Network.AWS.IAM.CreateLoginProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a password for the specified user, giving the user the ability to
-- access AWS services through the AWS Management Console. For more
-- information about managing passwords, see Managing Passwords in the Using
-- IAM guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateLoginProfile.html>
module Network.AWS.IAM.CreateLoginProfile
    (
    -- * Request
      CreateLoginProfile
    -- ** Request constructor
    , createLoginProfile
    -- ** Request lenses
    , clpPassword
    , clpPasswordResetRequired
    , clpUserName

    -- * Response
    , CreateLoginProfileResponse
    -- ** Response constructor
    , createLoginProfileResponse
    -- ** Response lenses
    , clprLoginProfile
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data CreateLoginProfile = CreateLoginProfile
    { _clpPassword              :: Sensitive Text
    , _clpPasswordResetRequired :: Maybe Bool
    , _clpUserName              :: Text
    } deriving (Eq, Ord, Show)

-- | 'CreateLoginProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clpPassword' @::@ 'Text'
--
-- * 'clpPasswordResetRequired' @::@ 'Maybe' 'Bool'
--
-- * 'clpUserName' @::@ 'Text'
--
createLoginProfile :: Text -- ^ 'clpUserName'
                   -> Text -- ^ 'clpPassword'
                   -> CreateLoginProfile
createLoginProfile p1 p2 = CreateLoginProfile
    { _clpUserName              = p1
    , _clpPassword              = withIso _Sensitive (const id) p2
    , _clpPasswordResetRequired = Nothing
    }

-- | The new password for the user.
clpPassword :: Lens' CreateLoginProfile Text
clpPassword = lens _clpPassword (\s a -> s { _clpPassword = a }) . _Sensitive

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
clpPasswordResetRequired :: Lens' CreateLoginProfile (Maybe Bool)
clpPasswordResetRequired =
    lens _clpPasswordResetRequired
        (\s a -> s { _clpPasswordResetRequired = a })

-- | The name of the user to create a password for.
clpUserName :: Lens' CreateLoginProfile Text
clpUserName = lens _clpUserName (\s a -> s { _clpUserName = a })

newtype CreateLoginProfileResponse = CreateLoginProfileResponse
    { _clprLoginProfile :: LoginProfile
    } deriving (Eq, Show)

-- | 'CreateLoginProfileResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clprLoginProfile' @::@ 'LoginProfile'
--
createLoginProfileResponse :: LoginProfile -- ^ 'clprLoginProfile'
                           -> CreateLoginProfileResponse
createLoginProfileResponse p1 = CreateLoginProfileResponse
    { _clprLoginProfile = p1
    }

-- | The user name and password create date.
clprLoginProfile :: Lens' CreateLoginProfileResponse LoginProfile
clprLoginProfile = lens _clprLoginProfile (\s a -> s { _clprLoginProfile = a })

instance ToPath CreateLoginProfile where
    toPath = const "/"

instance ToQuery CreateLoginProfile where
    toQuery CreateLoginProfile{..} = mconcat
        [ "Password"              =? _clpPassword
        , "PasswordResetRequired" =? _clpPasswordResetRequired
        , "UserName"              =? _clpUserName
        ]

instance ToHeaders CreateLoginProfile

query

instance AWSRequest CreateLoginProfile where
    type Sv CreateLoginProfile = IAM
    type Rs CreateLoginProfile = CreateLoginProfileResponse

    request  = post "CreateLoginProfile"
    response = xmlResponse

instance FromXML CreateLoginProfileResponse where
    parseXML = withElement "CreateLoginProfileResult" $ \x -> CreateLoginProfileResponse
        <$> x .@  "LoginProfile"
