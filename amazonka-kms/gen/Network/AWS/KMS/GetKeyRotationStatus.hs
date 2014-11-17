{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.KMS.GetKeyRotationStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves a Boolean value that indicates whether key rotation is enabled
-- for the specified key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GetKeyRotationStatus.html>
module Network.AWS.KMS.GetKeyRotationStatus
    (
    -- * Request
      GetKeyRotationStatus
    -- ** Request constructor
    , getKeyRotationStatus
    -- ** Request lenses
    , gkrsKeyId

    -- * Response
    , GetKeyRotationStatusResponse
    -- ** Response constructor
    , getKeyRotationStatusResponse
    -- ** Response lenses
    , gkrsrKeyRotationEnabled
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

newtype GetKeyRotationStatus = GetKeyRotationStatus
    { _gkrsKeyId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetKeyRotationStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gkrsKeyId' @::@ 'Text'
--
getKeyRotationStatus :: Text -- ^ 'gkrsKeyId'
                     -> GetKeyRotationStatus
getKeyRotationStatus p1 = GetKeyRotationStatus
    { _gkrsKeyId = p1
    }

-- | Unique identifier of the key. This can be an ARN, an alias, or a globally
-- unique identifier.
gkrsKeyId :: Lens' GetKeyRotationStatus Text
gkrsKeyId = lens _gkrsKeyId (\s a -> s { _gkrsKeyId = a })

newtype GetKeyRotationStatusResponse = GetKeyRotationStatusResponse
    { _gkrsrKeyRotationEnabled :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetKeyRotationStatusResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gkrsrKeyRotationEnabled' @::@ 'Maybe' 'Bool'
--
getKeyRotationStatusResponse :: GetKeyRotationStatusResponse
getKeyRotationStatusResponse = GetKeyRotationStatusResponse
    { _gkrsrKeyRotationEnabled = Nothing
    }

-- | A Boolean value that specifies whether key rotation is enabled.
gkrsrKeyRotationEnabled :: Lens' GetKeyRotationStatusResponse (Maybe Bool)
gkrsrKeyRotationEnabled =
    lens _gkrsrKeyRotationEnabled (\s a -> s { _gkrsrKeyRotationEnabled = a })

instance ToPath GetKeyRotationStatus where
    toPath = const "/"

instance ToQuery GetKeyRotationStatus where
    toQuery = const mempty

instance ToHeaders GetKeyRotationStatus
instance ToJSON GetKeyRotationStatus where
    toJSON = genericToJSON jsonOptions

instance AWSRequest GetKeyRotationStatus where
    type Sv GetKeyRotationStatus = KMS
    type Rs GetKeyRotationStatus = GetKeyRotationStatusResponse

    request  = post "GetKeyRotationStatus"
    response = jsonResponse

instance FromJSON GetKeyRotationStatusResponse where
    parseJSON = genericParseJSON jsonOptions
