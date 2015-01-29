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

-- Module      : Network.AWS.IAM.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the password policy for the AWS account.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccountPasswordPolicy.html>
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
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteAccountPasswordPolicy' constructor.
deleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy
deleteAccountPasswordPolicy = DeleteAccountPasswordPolicy

data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteAccountPasswordPolicyResponse' constructor.
deleteAccountPasswordPolicyResponse :: DeleteAccountPasswordPolicyResponse
deleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse

instance ToPath DeleteAccountPasswordPolicy where
    toPath = const "/"

instance ToQuery DeleteAccountPasswordPolicy where
    toQuery = const mempty

instance ToHeaders DeleteAccountPasswordPolicy

instance AWSRequest DeleteAccountPasswordPolicy where
    type Sv DeleteAccountPasswordPolicy = IAM
    type Rs DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicyResponse

    request  = post "DeleteAccountPasswordPolicy"
    response = nullResponse DeleteAccountPasswordPolicyResponse
