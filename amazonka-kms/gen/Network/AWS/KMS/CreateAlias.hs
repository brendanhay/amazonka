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

-- Module      : Network.AWS.KMS.CreateAlias
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a display name for a customer master key. An alias can be used to
-- identify a key and should be unique. The console enforces a one-to-one
-- mapping between the alias and a key. An alias name can contain only
-- alphanumeric characters, forward slashes (/), underscores (_), and dashes
-- (-). An alias must start with the word "alias" followed by a forward slash
-- (alias/). An alias that begins with "aws" after the forward slash
-- (alias/aws...) is reserved by Amazon Web Services (AWS).
module Network.AWS.KMS.CreateAlias
    (
    -- * Request
      CreateAlias
    -- ** Request constructor
    , createAlias
    -- ** Request lenses
    , caAliasName
    , caTargetKeyId

    -- * Response
    , CreateAliasResponse
    -- ** Response constructor
    , createAliasResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.KMS.Types

data CreateAlias = CreateAlias
    { _caAliasName   :: Text
    , _caTargetKeyId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateAlias' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caAliasName' @::@ 'Text'
--
-- * 'caTargetKeyId' @::@ 'Text'
--
createAlias :: Text -- ^ 'caAliasName'
            -> Text -- ^ 'caTargetKeyId'
            -> CreateAlias
createAlias p1 p2 = CreateAlias
    { _caAliasName   = p1
    , _caTargetKeyId = p2
    }

-- | String that contains the display name. Aliases that begin with AWS are
-- reserved.
caAliasName :: Lens' CreateAlias Text
caAliasName = lens _caAliasName (\s a -> s { _caAliasName = a })

-- | An identifier of the key for which you are creating the alias. This value
-- cannot be another alias.
caTargetKeyId :: Lens' CreateAlias Text
caTargetKeyId = lens _caTargetKeyId (\s a -> s { _caTargetKeyId = a })

instance ToPath CreateAlias where
    toPath = const "/"

instance ToQuery CreateAlias where
    toQuery = const mempty

instance ToHeaders CreateAlias

instance ToBody CreateAlias where
    toBody = toBody . encode . _caAliasName

data CreateAliasResponse = CreateAliasResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateAliasResponse' constructor.
createAliasResponse :: CreateAliasResponse
createAliasResponse = CreateAliasResponse

instance AWSRequest CreateAlias where
    type Sv CreateAlias = KMS
    type Rs CreateAlias = CreateAliasResponse

    request  = post
    response = nullaryResponse CreateAliasResponse
