{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- IAM guide. https://iam.amazonaws.com/ ?Action=CreateLoginProfile
-- &UserName=Bob &Password=Password1 &AUTHPARAMS Bob 2011-09-19T23:00:56Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.CreateLoginProfile
    (
    -- * Request
      CreateLoginProfile
    -- ** Request constructor
    , mkCreateLoginProfile
    -- ** Request lenses
    , clpUserName
    , clpPassword
    , clpPasswordResetRequired

    -- * Response
    , CreateLoginProfileResponse
    -- ** Response constructor
    , mkCreateLoginProfileResponse
    -- ** Response lenses
    , clprLoginProfile
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data CreateLoginProfile = CreateLoginProfile
    { _clpUserName :: !Text
    , _clpPassword :: !Text
    , _clpPasswordResetRequired :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLoginProfile' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
-- * @Password ::@ @Text@
--
-- * @PasswordResetRequired ::@ @Maybe Bool@
--
mkCreateLoginProfile :: Text -- ^ 'clpUserName'
                     -> Text -- ^ 'clpPassword'
                     -> CreateLoginProfile
mkCreateLoginProfile p1 p2 = CreateLoginProfile
    { _clpUserName = p1
    , _clpPassword = p2
    , _clpPasswordResetRequired = Nothing
    }

-- | Name of the user to create a password for.
clpUserName :: Lens' CreateLoginProfile Text
clpUserName = lens _clpUserName (\s a -> s { _clpUserName = a })

-- | The new password for the user.
clpPassword :: Lens' CreateLoginProfile Text
clpPassword = lens _clpPassword (\s a -> s { _clpPassword = a })

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
clpPasswordResetRequired :: Lens' CreateLoginProfile (Maybe Bool)
clpPasswordResetRequired =
    lens _clpPasswordResetRequired
         (\s a -> s { _clpPasswordResetRequired = a })

instance ToQuery CreateLoginProfile where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the CreateLoginProfile
-- action.
newtype CreateLoginProfileResponse = CreateLoginProfileResponse
    { _clprLoginProfile :: LoginProfile
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLoginProfileResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoginProfile ::@ @LoginProfile@
--
mkCreateLoginProfileResponse :: LoginProfile -- ^ 'clprLoginProfile'
                             -> CreateLoginProfileResponse
mkCreateLoginProfileResponse p1 = CreateLoginProfileResponse
    { _clprLoginProfile = p1
    }

-- | The user name and password create date.
clprLoginProfile :: Lens' CreateLoginProfileResponse LoginProfile
clprLoginProfile =
    lens _clprLoginProfile (\s a -> s { _clprLoginProfile = a })

instance FromXML CreateLoginProfileResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateLoginProfile where
    type Sv CreateLoginProfile = IAM
    type Rs CreateLoginProfile = CreateLoginProfileResponse

    request = post "CreateLoginProfile"
    response _ = xmlResponse
