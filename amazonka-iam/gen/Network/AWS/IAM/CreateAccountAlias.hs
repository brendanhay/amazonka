{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM
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
module Network.AWS.IAM
    (
    -- * Request
      CreateAccountAlias
    -- ** Request constructor
    , mkCreateAccountAlias
    -- ** Request lenses
    , caaAccountAlias

    -- * Response
    , CreateAccountAliasResponse
    -- ** Response constructor
    , mkCreateAccountAliasResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype CreateAccountAlias = CreateAccountAlias
    { _caaAccountAlias :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateAccountAlias' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AccountAlias ::@ @Text@
--
mkCreateAccountAlias :: Text -- ^ 'caaAccountAlias'
                     -> CreateAccountAlias
mkCreateAccountAlias p1 = CreateAccountAlias
    { _caaAccountAlias = p1
    }

-- | Name of the account alias to create.
caaAccountAlias :: Lens' CreateAccountAlias Text
caaAccountAlias = lens _caaAccountAlias (\s a -> s { _caaAccountAlias = a })

instance ToQuery CreateAccountAlias where
    toQuery = genericQuery def

data CreateAccountAliasResponse = CreateAccountAliasResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateAccountAliasResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCreateAccountAliasResponse :: CreateAccountAliasResponse
mkCreateAccountAliasResponse = CreateAccountAliasResponse

instance AWSRequest CreateAccountAlias where
    type Sv CreateAccountAlias = IAM
    type Rs CreateAccountAlias = CreateAccountAliasResponse

    request = post "CreateAccountAlias"
    response _ = nullaryResponse CreateAccountAliasResponse
