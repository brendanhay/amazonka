{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the password policy for the AWS account. https://iam.amazonaws.com/
-- ?Action=DeleteAccountPasswordPolicy &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeleteAccountPasswordPolicy where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy
    deriving (Eq, Show, Generic)

makeLenses ''DeleteAccountPasswordPolicy

instance ToQuery DeleteAccountPasswordPolicy where
    toQuery = genericToQuery def

data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteAccountPasswordPolicyResponse

instance AWSRequest DeleteAccountPasswordPolicy where
    type Sv DeleteAccountPasswordPolicy = IAM
    type Rs DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicyResponse

    request = post "DeleteAccountPasswordPolicy"
    response _ _ = return (Right DeleteAccountPasswordPolicyResponse)
