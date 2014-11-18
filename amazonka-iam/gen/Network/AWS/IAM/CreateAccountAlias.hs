{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateAccountAlias
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an alias for your AWS account. For information about using an AWS
-- account alias, see Using an Alias for Your AWS Account ID in the Using IAM
-- guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateAccountAlias.html>
module Network.AWS.IAM.CreateAccountAlias
    (
    -- * Request
      CreateAccountAlias
    -- ** Request constructor
    , createAccountAlias
    -- ** Request lenses
    , caaAccountAlias

    -- * Response
    , CreateAccountAliasResponse
    -- ** Response constructor
    , createAccountAliasResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype CreateAccountAlias = CreateAccountAlias
    { _caaAccountAlias :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'CreateAccountAlias' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caaAccountAlias' @::@ 'Text'
--
createAccountAlias :: Text -- ^ 'caaAccountAlias'
                   -> CreateAccountAlias
createAccountAlias p1 = CreateAccountAlias
    { _caaAccountAlias = p1
    }

-- | The name of the account alias to create.
caaAccountAlias :: Lens' CreateAccountAlias Text
caaAccountAlias = lens _caaAccountAlias (\s a -> s { _caaAccountAlias = a })

data CreateAccountAliasResponse = CreateAccountAliasResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateAccountAliasResponse' constructor.
createAccountAliasResponse :: CreateAccountAliasResponse
createAccountAliasResponse = CreateAccountAliasResponse

instance ToPath CreateAccountAlias where
    toPath = const "/"

instance ToQuery CreateAccountAlias

instance ToHeaders CreateAccountAlias

instance AWSRequest CreateAccountAlias where
    type Sv CreateAccountAlias = IAM
    type Rs CreateAccountAlias = CreateAccountAliasResponse

    request  = post "CreateAccountAlias"
    response = nullResponse CreateAccountAliasResponse
