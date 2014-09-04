{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.CreateLoginProfile
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
-- IAM guide. https://iam.amazonaws.com/ ?Action=CreateLoginProfile
-- &UserName=Bob &Password=Password1 &AUTHPARAMS Bob 2011-09-19T23:00:56Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.CreateLoginProfile
    (
    -- * Request
      CreateLoginProfile
    -- ** Request constructor
    , mkCreateLoginProfileRequest
    -- ** Request lenses
    , clprUserName
    , clprPassword
    , clprPasswordResetRequired

    -- * Response
    , CreateLoginProfileResponse
    -- ** Response lenses
    , clpsLoginProfile
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLoginProfile' request.
mkCreateLoginProfileRequest :: Text -- ^ 'clprUserName'
                            -> Text -- ^ 'clprPassword'
                            -> CreateLoginProfile
mkCreateLoginProfileRequest p1 p2 = CreateLoginProfile
    { _clprUserName = p1
    , _clprPassword = p2
    , _clprPasswordResetRequired = Nothing
    }
{-# INLINE mkCreateLoginProfileRequest #-}

data CreateLoginProfile = CreateLoginProfile
    { _clprUserName :: Text
      -- ^ Name of the user to create a password for.
    , _clprPassword :: Text
      -- ^ The new password for the user.
    , _clprPasswordResetRequired :: Maybe Bool
      -- ^ Specifies whether the user is required to set a new password on
      -- next sign-in.
    } deriving (Show, Generic)

-- | Name of the user to create a password for.
clprUserName :: Lens' CreateLoginProfile (Text)
clprUserName = lens _clprUserName (\s a -> s { _clprUserName = a })
{-# INLINE clprUserName #-}

-- | The new password for the user.
clprPassword :: Lens' CreateLoginProfile (Text)
clprPassword = lens _clprPassword (\s a -> s { _clprPassword = a })
{-# INLINE clprPassword #-}

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
clprPasswordResetRequired :: Lens' CreateLoginProfile (Maybe Bool)
clprPasswordResetRequired = lens _clprPasswordResetRequired (\s a -> s { _clprPasswordResetRequired = a })
{-# INLINE clprPasswordResetRequired #-}

instance ToQuery CreateLoginProfile where
    toQuery = genericQuery def

newtype CreateLoginProfileResponse = CreateLoginProfileResponse
    { _clpsLoginProfile :: LoginProfile
      -- ^ The user name and password create date.
    } deriving (Show, Generic)

-- | The user name and password create date.
clpsLoginProfile :: Lens' CreateLoginProfileResponse (LoginProfile)
clpsLoginProfile = lens _clpsLoginProfile (\s a -> s { _clpsLoginProfile = a })
{-# INLINE clpsLoginProfile #-}

instance FromXML CreateLoginProfileResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateLoginProfile where
    type Sv CreateLoginProfile = IAM
    type Rs CreateLoginProfile = CreateLoginProfileResponse

    request = post "CreateLoginProfile"
    response _ = xmlResponse
