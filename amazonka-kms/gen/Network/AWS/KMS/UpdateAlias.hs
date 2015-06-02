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

-- Module      : Network.AWS.KMS.UpdateAlias
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates an alias to associate it with a different key.
--
-- An alias name can contain only alphanumeric characters, forward slashes (/),
-- underscores (_), and dashes (-). An alias must start with the word "alias"
-- followed by a forward slash (alias/). An alias that begins with "aws" after
-- the forward slash (alias/aws...) is reserved by Amazon Web Services (AWS).
--
-- An alias is not a property of a key. Therefore, an alias can be associated
-- with and disassociated from an existing key without changing the properties
-- of the key.
--
-- Note that you cannot create or update an alias that represents a key in
-- another account.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_UpdateAlias.html>
module Network.AWS.KMS.UpdateAlias
    (
    -- * Request
      UpdateAlias
    -- ** Request constructor
    , updateAlias
    -- ** Request lenses
    , uaAliasName
    , uaTargetKeyId

    -- * Response
    , UpdateAliasResponse
    -- ** Response constructor
    , updateAliasResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data UpdateAlias = UpdateAlias
    { _uaAliasName   :: Text
    , _uaTargetKeyId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateAlias' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaAliasName' @::@ 'Text'
--
-- * 'uaTargetKeyId' @::@ 'Text'
--
updateAlias :: Text -- ^ 'uaAliasName'
            -> Text -- ^ 'uaTargetKeyId'
            -> UpdateAlias
updateAlias p1 p2 = UpdateAlias
    { _uaAliasName   = p1
    , _uaTargetKeyId = p2
    }

-- | String that contains the name of the alias to be modifed. The name must start
-- with the word "alias" followed by a forward slash (alias/). Aliases that
-- begin with "alias/AWS" are reserved.
uaAliasName :: Lens' UpdateAlias Text
uaAliasName = lens _uaAliasName (\s a -> s { _uaAliasName = a })

-- | Unique identifier of the customer master key to be associated with the alias.
-- This value can be a globally unique identifier or the fully specified ARN of
-- a key.  Key ARN Example -
-- arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012 Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
--
uaTargetKeyId :: Lens' UpdateAlias Text
uaTargetKeyId = lens _uaTargetKeyId (\s a -> s { _uaTargetKeyId = a })

data UpdateAliasResponse = UpdateAliasResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'UpdateAliasResponse' constructor.
updateAliasResponse :: UpdateAliasResponse
updateAliasResponse = UpdateAliasResponse

instance ToPath UpdateAlias where
    toPath = const "/"

instance ToQuery UpdateAlias where
    toQuery = const mempty

instance ToHeaders UpdateAlias

instance ToJSON UpdateAlias where
    toJSON UpdateAlias{..} = object
        [ "AliasName"   .= _uaAliasName
        , "TargetKeyId" .= _uaTargetKeyId
        ]

instance AWSRequest UpdateAlias where
    type Sv UpdateAlias = KMS
    type Rs UpdateAlias = UpdateAliasResponse

    request  = post "UpdateAlias"
    response = nullResponse UpdateAliasResponse
