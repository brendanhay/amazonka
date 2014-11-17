{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetAccountPasswordPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the password policy for the AWS account. For more information
-- about using a password policy, go to Managing an IAM Password Policy.
--
-- <GetAccountPasswordPolicy.html>
module Network.AWS.IAM.GetAccountPasswordPolicy
    (
    -- * Request
      GetAccountPasswordPolicy
    -- ** Request constructor
    , getAccountPasswordPolicy

    -- * Response
    , GetAccountPasswordPolicyResponse
    -- ** Response constructor
    , getAccountPasswordPolicyResponse
    -- ** Response lenses
    , gapprPasswordPolicy
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data GetAccountPasswordPolicy = GetAccountPasswordPolicy
    deriving (Eq, Ord, Show, Generic)

-- | 'GetAccountPasswordPolicy' constructor.
getAccountPasswordPolicy :: GetAccountPasswordPolicy
getAccountPasswordPolicy = GetAccountPasswordPolicy

newtype GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse
    { _gapprPasswordPolicy :: PasswordPolicy
    } deriving (Eq, Show, Generic)

-- | 'GetAccountPasswordPolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gapprPasswordPolicy' @::@ 'PasswordPolicy'
--
getAccountPasswordPolicyResponse :: PasswordPolicy -- ^ 'gapprPasswordPolicy'
                                 -> GetAccountPasswordPolicyResponse
getAccountPasswordPolicyResponse p1 = GetAccountPasswordPolicyResponse
    { _gapprPasswordPolicy = p1
    }

gapprPasswordPolicy :: Lens' GetAccountPasswordPolicyResponse PasswordPolicy
gapprPasswordPolicy =
    lens _gapprPasswordPolicy (\s a -> s { _gapprPasswordPolicy = a })

instance AWSRequest GetAccountPasswordPolicy where
    type Sv GetAccountPasswordPolicy = IAM
    type Rs GetAccountPasswordPolicy = GetAccountPasswordPolicyResponse

    request  = post "GetAccountPasswordPolicy"
    response = xmlResponse

instance FromXML GetAccountPasswordPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetAccountPasswordPolicyResponse"

instance ToPath GetAccountPasswordPolicy where
    toPath = const "/"

instance ToHeaders GetAccountPasswordPolicy

instance ToQuery GetAccountPasswordPolicy
