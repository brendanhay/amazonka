{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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

data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy

-- | 'DeleteAccountPasswordPolicy' constructor.
deleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy
deleteAccountPasswordPolicy = DeleteAccountPasswordPolicy

instance ToPath DeleteAccountPasswordPolicy where
    toPath = const "/"

instance ToQuery DeleteAccountPasswordPolicy

data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse

-- | 'DeleteAccountPasswordPolicyResponse' constructor.
deleteAccountPasswordPolicyResponse :: DeleteAccountPasswordPolicyResponse
deleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse

instance AWSRequest DeleteAccountPasswordPolicy where
    type Sv DeleteAccountPasswordPolicy = IAM
    type Rs DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicyResponse

    request  = post "DeleteAccountPasswordPolicy"
    response = const (nullaryResponse DeleteAccountPasswordPolicyResponse)
