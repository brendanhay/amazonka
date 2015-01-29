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

-- Module      : Network.AWS.KMS.DescribeKey
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

-- | Provides detailed information about the specified customer master key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html>
module Network.AWS.KMS.DescribeKey
    (
    -- * Request
      DescribeKey
    -- ** Request constructor
    , describeKey
    -- ** Request lenses
    , dk1KeyId

    -- * Response
    , DescribeKeyResponse
    -- ** Response constructor
    , describeKeyResponse
    -- ** Response lenses
    , dkrKeyMetadata
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

newtype DescribeKey = DescribeKey
    { _dk1KeyId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DescribeKey' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dk1KeyId' @::@ 'Text'
--
describeKey :: Text -- ^ 'dk1KeyId'
            -> DescribeKey
describeKey p1 = DescribeKey
    { _dk1KeyId = p1
    }

-- | A unique identifier for the customer master key. This value can be a globally
-- unique identifier, a fully specified ARN to either an alias or a key, or an
-- alias name prefixed by "alias/".  Key ARN Example -
-- arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012 Alias ARN Example - arn:aws:kms:us-east-1:123456789012:/alias/MyAliasName
-- Globally Unique Key ID Example - 12345678-1234-1234-123456789012 Alias Name
-- Example - alias/MyAliasName
dk1KeyId :: Lens' DescribeKey Text
dk1KeyId = lens _dk1KeyId (\s a -> s { _dk1KeyId = a })

newtype DescribeKeyResponse = DescribeKeyResponse
    { _dkrKeyMetadata :: Maybe KeyMetadata
    } deriving (Eq, Read, Show)

-- | 'DescribeKeyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkrKeyMetadata' @::@ 'Maybe' 'KeyMetadata'
--
describeKeyResponse :: DescribeKeyResponse
describeKeyResponse = DescribeKeyResponse
    { _dkrKeyMetadata = Nothing
    }

-- | Metadata associated with the key.
dkrKeyMetadata :: Lens' DescribeKeyResponse (Maybe KeyMetadata)
dkrKeyMetadata = lens _dkrKeyMetadata (\s a -> s { _dkrKeyMetadata = a })

instance ToPath DescribeKey where
    toPath = const "/"

instance ToQuery DescribeKey where
    toQuery = const mempty

instance ToHeaders DescribeKey

instance ToJSON DescribeKey where
    toJSON DescribeKey{..} = object
        [ "KeyId" .= _dk1KeyId
        ]

instance AWSRequest DescribeKey where
    type Sv DescribeKey = KMS
    type Rs DescribeKey = DescribeKeyResponse

    request  = post "DescribeKey"
    response = jsonResponse

instance FromJSON DescribeKeyResponse where
    parseJSON = withObject "DescribeKeyResponse" $ \o -> DescribeKeyResponse
        <$> o .:? "KeyMetadata"
