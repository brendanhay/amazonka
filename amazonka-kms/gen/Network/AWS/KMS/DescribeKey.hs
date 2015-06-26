{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.KMS.DescribeKey
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    , desKeyId

    -- * Response
    , DescribeKeyResponse
    -- ** Response constructor
    , describeKeyResponse
    -- ** Response lenses
    , dkrKeyMetadata
    , dkrStatusCode
    ) where

import Network.AWS.KMS.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desKeyId'
newtype DescribeKey = DescribeKey'{_desKeyId :: Text} deriving (Eq, Read, Show)

-- | 'DescribeKey' smart constructor.
describeKey :: Text -> DescribeKey
describeKey pKeyId = DescribeKey'{_desKeyId = pKeyId};

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier, a fully specified ARN to either an alias or
-- a key, or an alias name prefixed by \"alias\/\".
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Alias ARN Example -
--     arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
-- -   Alias Name Example - alias\/MyAliasName
desKeyId :: Lens' DescribeKey Text
desKeyId = lens _desKeyId (\ s a -> s{_desKeyId = a});

instance AWSRequest DescribeKey where
        type Sv DescribeKey = KMS
        type Rs DescribeKey = DescribeKeyResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeKeyResponse' <$>
                   (x .?> "KeyMetadata") <*> (pure (fromEnum s)))

instance ToHeaders DescribeKey where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.DescribeKey" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeKey where
        toJSON DescribeKey'{..}
          = object ["KeyId" .= _desKeyId]

instance ToPath DescribeKey where
        toPath = const "/"

instance ToQuery DescribeKey where
        toQuery = const mempty

-- | /See:/ 'describeKeyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkrKeyMetadata'
--
-- * 'dkrStatusCode'
data DescribeKeyResponse = DescribeKeyResponse'{_dkrKeyMetadata :: Maybe KeyMetadata, _dkrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeKeyResponse' smart constructor.
describeKeyResponse :: Int -> DescribeKeyResponse
describeKeyResponse pStatusCode = DescribeKeyResponse'{_dkrKeyMetadata = Nothing, _dkrStatusCode = pStatusCode};

-- | Metadata associated with the key.
dkrKeyMetadata :: Lens' DescribeKeyResponse (Maybe KeyMetadata)
dkrKeyMetadata = lens _dkrKeyMetadata (\ s a -> s{_dkrKeyMetadata = a});

-- | FIXME: Undocumented member.
dkrStatusCode :: Lens' DescribeKeyResponse Int
dkrStatusCode = lens _dkrStatusCode (\ s a -> s{_dkrStatusCode = a});
