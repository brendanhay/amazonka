{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the password policy for the AWS account.
--
-- <DeleteAccountPasswordPolicy.html>
module Network.AWS.IAM.DeleteAccountPasswordPolicy
    (
    -- * Request
      DeleteAccountPasswordPolicy
    -- ** Request constructor
    , deleteAccountPasswordPolicy

    -- * Response
    , DeleteAccountPasswordPolicyResponse
    -- ** Response constructor
    , deleteAccountPasswordPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteAccountPasswordPolicy' constructor.
deleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy
deleteAccountPasswordPolicy = DeleteAccountPasswordPolicy

data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteAccountPasswordPolicyResponse' constructor.
deleteAccountPasswordPolicyResponse :: DeleteAccountPasswordPolicyResponse
deleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse

instance AWSRequest DeleteAccountPasswordPolicy where
    type Sv DeleteAccountPasswordPolicy = IAM
    type Rs DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicyResponse

    request  = post "DeleteAccountPasswordPolicy"
    response = nullResponse DeleteAccountPasswordPolicyResponse

instance ToPath DeleteAccountPasswordPolicy where
    toPath = const "/"

instance ToHeaders DeleteAccountPasswordPolicy

instance ToQuery DeleteAccountPasswordPolicy
