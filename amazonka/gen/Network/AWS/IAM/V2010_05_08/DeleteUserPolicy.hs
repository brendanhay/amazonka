{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteUserPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified policy associated with the specified user.
-- https://iam.amazonaws.com/ ?Action=DeleteUserPolicy &UserName=Bob
-- &PolicyName=AllAccessPolicy &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeleteUserPolicy where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data DeleteUserPolicy = DeleteUserPolicy
    { _duprUserName :: Text
      -- ^ Name of the user the policy is associated with.
    , _duprPolicyName :: Text
      -- ^ Name of the policy document to delete.
    } deriving (Show, Generic)

makeLenses ''DeleteUserPolicy

instance ToQuery DeleteUserPolicy where
    toQuery = genericToQuery def

data DeleteUserPolicyResponse = DeleteUserPolicyResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteUserPolicyResponse

instance AWSRequest DeleteUserPolicy where
    type Sv DeleteUserPolicy = IAM
    type Rs DeleteUserPolicy = DeleteUserPolicyResponse

    request = post "DeleteUserPolicy"
    response _ = nullaryResponse DeleteUserPolicyResponse
