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

-- Module      : Network.AWS.IAM.UpdateLoginProfile
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

-- | Changes the password for the specified user.
--
-- Users can change their own passwords by calling 'ChangePassword'. For more
-- information about modifying passwords, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords> in the /Using IAM/
-- guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateLoginProfile.html>
module Network.AWS.IAM.UpdateLoginProfile
    (
    -- * Request
      UpdateLoginProfile
    -- ** Request constructor
    , updateLoginProfile
    -- ** Request lenses
    , ulpPassword
    , ulpPasswordResetRequired
    , ulpUserName

    -- * Response
    , UpdateLoginProfileResponse
    -- ** Response constructor
    , updateLoginProfileResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data UpdateLoginProfile = UpdateLoginProfile
    { _ulpPassword              :: Maybe (Sensitive Text)
    , _ulpPasswordResetRequired :: Maybe Bool
    , _ulpUserName              :: Text
    } deriving (Eq, Ord, Show)

-- | 'UpdateLoginProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ulpPassword' @::@ 'Maybe' 'Text'
--
-- * 'ulpPasswordResetRequired' @::@ 'Maybe' 'Bool'
--
-- * 'ulpUserName' @::@ 'Text'
--
updateLoginProfile :: Text -- ^ 'ulpUserName'
                   -> UpdateLoginProfile
updateLoginProfile p1 = UpdateLoginProfile
    { _ulpUserName              = p1
    , _ulpPassword              = Nothing
    , _ulpPasswordResetRequired = Nothing
    }

-- | The new password for the specified user.
ulpPassword :: Lens' UpdateLoginProfile (Maybe Text)
ulpPassword = lens _ulpPassword (\s a -> s { _ulpPassword = a }) . mapping _Sensitive

-- | Require the specified user to set a new password on next sign-in.
ulpPasswordResetRequired :: Lens' UpdateLoginProfile (Maybe Bool)
ulpPasswordResetRequired =
    lens _ulpPasswordResetRequired
        (\s a -> s { _ulpPasswordResetRequired = a })

-- | The name of the user whose password you want to update.
ulpUserName :: Lens' UpdateLoginProfile Text
ulpUserName = lens _ulpUserName (\s a -> s { _ulpUserName = a })

data UpdateLoginProfileResponse = UpdateLoginProfileResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateLoginProfileResponse' constructor.
updateLoginProfileResponse :: UpdateLoginProfileResponse
updateLoginProfileResponse = UpdateLoginProfileResponse

instance ToPath UpdateLoginProfile where
    toPath = const "/"

instance ToQuery UpdateLoginProfile where
    toQuery UpdateLoginProfile{..} = mconcat
        [ "Password"              =? _ulpPassword
        , "PasswordResetRequired" =? _ulpPasswordResetRequired
        , "UserName"              =? _ulpUserName
        ]

instance ToHeaders UpdateLoginProfile

instance AWSRequest UpdateLoginProfile where
    type Sv UpdateLoginProfile = IAM
    type Rs UpdateLoginProfile = UpdateLoginProfileResponse

    request  = post "UpdateLoginProfile"
    response = nullResponse UpdateLoginProfileResponse
