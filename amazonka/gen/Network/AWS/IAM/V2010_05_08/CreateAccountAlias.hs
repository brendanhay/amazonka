{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.IAM.V2010_05_08.CreateAccountAlias
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action creates an alias for your AWS account. For information about
-- using an AWS account alias, see Using an Alias for Your AWS Account ID in
-- the Using IAM guide. https://iam.amazonaws.com/ ?Action=CreateAccountAlias
-- &AccountAlias=foocorporation &Version=2010-05-08 &AUTHPARAMS
-- 36b5db08-f1b0-11df-8fbe-45274EXAMPLE.
module Network.AWS.IAM.V2010_05_08.CreateAccountAlias where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data CreateAccountAlias = CreateAccountAlias
    { _caarAccountAlias :: Text
      -- ^ Name of the account alias to create.
    } deriving (Generic)

instance ToQuery CreateAccountAlias where
    toQuery = genericToQuery def

instance AWSRequest CreateAccountAlias where
    type Sv CreateAccountAlias = IAM
    type Rs CreateAccountAlias = CreateAccountAliasResponse

    request = post "CreateAccountAlias"
    response _ _ = return (Right CreateAccountAliasResponse)

data CreateAccountAliasResponse = CreateAccountAliasResponse
    deriving (Eq, Show, Generic)
