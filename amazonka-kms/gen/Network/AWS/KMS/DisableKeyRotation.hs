{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
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

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

newtype DisableKeyRotation = DisableKeyRotation
    { _dkrKeyId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

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

-- | Unique identifier of the customer master key for which rotation is to be
-- disabled. This can be an ARN, an alias, or a globally unique identifier.
dkrKeyId :: Lens' DisableKeyRotation Text
dkrKeyId = lens _dkrKeyId (\s a -> s { _dkrKeyId = a })

data DisableKeyRotationResponse = DisableKeyRotationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DisableKeyRotationResponse' constructor.
disableKeyRotationResponse :: DisableKeyRotationResponse
disableKeyRotationResponse = DisableKeyRotationResponse

instance ToPath DisableKeyRotation where
    toPath = const "/"

instance ToQuery DisableKeyRotation where
    toQuery = const mempty

instance ToHeaders DisableKeyRotation
instance ToJSON DisableKeyRotation where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DisableKeyRotation where
    type Sv DisableKeyRotation = KMS
    type Rs DisableKeyRotation = DisableKeyRotationResponse

    request  = post
    response = nullResponse DisableKeyRotationResponse
