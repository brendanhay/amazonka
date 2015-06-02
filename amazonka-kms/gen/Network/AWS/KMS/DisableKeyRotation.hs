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

-- Module      : Network.AWS.KMS.DisableKeyRotation
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

-- | Disables rotation of the specified key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_DisableKeyRotation.html>
module Network.AWS.KMS.DisableKeyRotation
    (
    -- * Request
      DisableKeyRotation
    -- ** Request constructor
    , disableKeyRotation
    -- ** Request lenses
    , dkrKeyId

    -- * Response
    , DisableKeyRotationResponse
    -- ** Response constructor
    , disableKeyRotationResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

newtype DisableKeyRotation = DisableKeyRotation
    { _dkrKeyId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DisableKeyRotation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkrKeyId' @::@ 'Text'
--
disableKeyRotation :: Text -- ^ 'dkrKeyId'
                   -> DisableKeyRotation
disableKeyRotation p1 = DisableKeyRotation
    { _dkrKeyId = p1
    }

-- | A unique identifier for the customer master key. This value can be a globally
-- unique identifier or the fully specified ARN to a key.  Key ARN Example -
-- arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012 Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
--
dkrKeyId :: Lens' DisableKeyRotation Text
dkrKeyId = lens _dkrKeyId (\s a -> s { _dkrKeyId = a })

data DisableKeyRotationResponse = DisableKeyRotationResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DisableKeyRotationResponse' constructor.
disableKeyRotationResponse :: DisableKeyRotationResponse
disableKeyRotationResponse = DisableKeyRotationResponse

instance ToPath DisableKeyRotation where
    toPath = const "/"

instance ToQuery DisableKeyRotation where
    toQuery = const mempty

instance ToHeaders DisableKeyRotation

instance ToJSON DisableKeyRotation where
    toJSON DisableKeyRotation{..} = object
        [ "KeyId" .= _dkrKeyId
        ]

instance AWSRequest DisableKeyRotation where
    type Sv DisableKeyRotation = KMS
    type Rs DisableKeyRotation = DisableKeyRotationResponse

    request  = post "DisableKeyRotation"
    response = nullResponse DisableKeyRotationResponse
