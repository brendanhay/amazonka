{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.KMS.DisableKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Marks a key as disabled, thereby preventing its use.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_DisableKey.html>
module Network.AWS.KMS.DisableKey
    (
    -- * Request
      DisableKey
    -- ** Request constructor
    , disableKey
    -- ** Request lenses
    , dkKeyId

    -- * Response
    , DisableKeyResponse
    -- ** Response constructor
    , disableKeyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

newtype DisableKey = DisableKey
    { _dkKeyId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DisableKey' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkKeyId' @::@ 'Text'
--
disableKey :: Text -- ^ 'dkKeyId'
           -> DisableKey
disableKey p1 = DisableKey
    { _dkKeyId = p1
    }

-- | Unique identifier of the customer master key to be disabled. This can be
-- an ARN, an alias, or a globally unique identifier.
dkKeyId :: Lens' DisableKey Text
dkKeyId = lens _dkKeyId (\s a -> s { _dkKeyId = a })

data DisableKeyResponse = DisableKeyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DisableKeyResponse' constructor.
disableKeyResponse :: DisableKeyResponse
disableKeyResponse = DisableKeyResponse

instance ToPath DisableKey where
    toPath = const "/"

instance ToQuery DisableKey where
    toQuery = const mempty

instance ToHeaders DisableKey

instance ToJSON DisableKey where
    toJSON DisableKey{..} = object
        [ "KeyId" .= _dkKeyId
        ]

instance AWSRequest DisableKey where
    type Sv DisableKey = KMS
    type Rs DisableKey = DisableKeyResponse

    request  = post "DisableKey"
    response = nullResponse DisableKeyResponse
