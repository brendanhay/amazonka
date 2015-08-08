{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DescribeKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provides detailed information about the specified customer master key.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html AWS API Reference> for DescribeKey.
module Network.AWS.KMS.DescribeKey
    (
    -- * Creating a Request
      DescribeKey
    , describeKey
    -- * Request Lenses
    , dKeyId

    -- * Destructuring the Response
    , DescribeKeyResponse
    , describeKeyResponse
    -- * Response Lenses
    , dkrsKeyMetadata
    , dkrsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dKeyId'
newtype DescribeKey = DescribeKey'
    { _dKeyId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeKey' smart constructor.
describeKey :: Text -> DescribeKey
describeKey pKeyId_ =
    DescribeKey'
    { _dKeyId = pKeyId_
    }

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
dKeyId :: Lens' DescribeKey Text
dKeyId = lens _dKeyId (\ s a -> s{_dKeyId = a});

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
        toJSON DescribeKey'{..} = object ["KeyId" .= _dKeyId]

instance ToPath DescribeKey where
        toPath = const "/"

instance ToQuery DescribeKey where
        toQuery = const mempty

-- | /See:/ 'describeKeyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkrsKeyMetadata'
--
-- * 'dkrsStatus'
data DescribeKeyResponse = DescribeKeyResponse'
    { _dkrsKeyMetadata :: !(Maybe KeyMetadata)
    , _dkrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeKeyResponse' smart constructor.
describeKeyResponse :: Int -> DescribeKeyResponse
describeKeyResponse pStatus_ =
    DescribeKeyResponse'
    { _dkrsKeyMetadata = Nothing
    , _dkrsStatus = pStatus_
    }

-- | Metadata associated with the key.
dkrsKeyMetadata :: Lens' DescribeKeyResponse (Maybe KeyMetadata)
dkrsKeyMetadata = lens _dkrsKeyMetadata (\ s a -> s{_dkrsKeyMetadata = a});

-- | Undocumented member.
dkrsStatus :: Lens' DescribeKeyResponse Int
dkrsStatus = lens _dkrsStatus (\ s a -> s{_dkrsStatus = a});
