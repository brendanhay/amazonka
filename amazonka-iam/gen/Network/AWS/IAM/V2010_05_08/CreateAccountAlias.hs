{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.IAM.V2010_05_08.CreateAccountAlias
    (
    -- * Request
      CreateAccountAlias
    -- ** Request constructor
    , createAccountAlias
    -- ** Request lenses
    , caarAccountAlias

    -- * Response
    , CreateAccountAliasResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateAccountAlias' request.
createAccountAlias :: Text -- ^ 'caarAccountAlias'
                   -> CreateAccountAlias
createAccountAlias p1 = CreateAccountAlias
    { _caarAccountAlias = p1
    }
{-# INLINE createAccountAlias #-}

data CreateAccountAlias = CreateAccountAlias
    { _caarAccountAlias :: Text
      -- ^ Name of the account alias to create.
    } deriving (Show, Generic)

-- | Name of the account alias to create.
caarAccountAlias :: Lens' CreateAccountAlias (Text)
caarAccountAlias f x =
    f (_caarAccountAlias x)
        <&> \y -> x { _caarAccountAlias = y }
{-# INLINE caarAccountAlias #-}

instance ToQuery CreateAccountAlias where
    toQuery = genericQuery def

data CreateAccountAliasResponse = CreateAccountAliasResponse
    deriving (Eq, Show, Generic)

instance AWSRequest CreateAccountAlias where
    type Sv CreateAccountAlias = IAM
    type Rs CreateAccountAlias = CreateAccountAliasResponse

    request = post "CreateAccountAlias"
    response _ = nullaryResponse CreateAccountAliasResponse
