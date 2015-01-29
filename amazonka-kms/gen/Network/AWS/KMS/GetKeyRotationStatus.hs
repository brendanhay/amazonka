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

-- Module      : Network.AWS.KMS.GetKeyRotationStatus
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

-- | Retrieves a Boolean value that indicates whether key rotation is enabled for
-- the specified key.
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
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

-- | A unique identifier for the customer master key. This value can be a globally
-- unique identifier or the fully specified ARN to a key.  Key ARN Example -
-- arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012 Globally Unique Key ID Example - 12345678-1234-1234-123456789012
--
gkrsKeyId :: Lens' GetKeyRotationStatus Text
gkrsKeyId = lens _gkrsKeyId (\s a -> s { _gkrsKeyId = a })

newtype GetKeyRotationStatusResponse = GetKeyRotationStatusResponse
    { _gkrsrKeyRotationEnabled :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

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
    toJSON GetKeyRotationStatus{..} = object
        [ "KeyId" .= _gkrsKeyId
        ]

instance AWSRequest GetKeyRotationStatus where
    type Sv GetKeyRotationStatus = KMS
    type Rs GetKeyRotationStatus = GetKeyRotationStatusResponse

    request  = post "GetKeyRotationStatus"
    response = jsonResponse

instance FromJSON GetKeyRotationStatusResponse where
    parseJSON = withObject "GetKeyRotationStatusResponse" $ \o -> GetKeyRotationStatusResponse
        <$> o .:? "KeyRotationEnabled"
