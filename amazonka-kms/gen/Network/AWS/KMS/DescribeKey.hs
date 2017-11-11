{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DescribeKey
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides detailed information about the specified customer master key.
--
--
module Network.AWS.KMS.DescribeKey
    (
    -- * Creating a Request
      describeKey
    , DescribeKey
    -- * Request Lenses
    , dGrantTokens
    , dKeyId

    -- * Destructuring the Response
    , describeKeyResponse
    , DescribeKeyResponse
    -- * Response Lenses
    , dkrsKeyMetadata
    , dkrsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeKey' smart constructor.
data DescribeKey = DescribeKey'
  { _dGrantTokens :: {-# NOUNPACK #-}!(Maybe [Text])
  , _dKeyId       :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dGrantTokens' - A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'dKeyId' - A unique identifier for the customer master key. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Alias ARN Example - arn:aws:kms:us-east-1:123456789012:alias/MyAliasName     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012     * Alias Name Example - alias/MyAliasName
describeKey
    :: Text -- ^ 'dKeyId'
    -> DescribeKey
describeKey pKeyId_ = DescribeKey' {_dGrantTokens = Nothing, _dKeyId = pKeyId_}


-- | A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
dGrantTokens :: Lens' DescribeKey [Text]
dGrantTokens = lens _dGrantTokens (\ s a -> s{_dGrantTokens = a}) . _Default . _Coerce;

-- | A unique identifier for the customer master key. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Alias ARN Example - arn:aws:kms:us-east-1:123456789012:alias/MyAliasName     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012     * Alias Name Example - alias/MyAliasName
dKeyId :: Lens' DescribeKey Text
dKeyId = lens _dKeyId (\ s a -> s{_dKeyId = a});

instance AWSRequest DescribeKey where
        type Rs DescribeKey = DescribeKeyResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeKeyResponse' <$>
                   (x .?> "KeyMetadata") <*> (pure (fromEnum s)))

instance Hashable DescribeKey where

instance NFData DescribeKey where

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
          = object
              (catMaybes
                 [("GrantTokens" .=) <$> _dGrantTokens,
                  Just ("KeyId" .= _dKeyId)])

instance ToPath DescribeKey where
        toPath = const "/"

instance ToQuery DescribeKey where
        toQuery = const mempty

-- | /See:/ 'describeKeyResponse' smart constructor.
data DescribeKeyResponse = DescribeKeyResponse'
  { _dkrsKeyMetadata    :: {-# NOUNPACK #-}!(Maybe KeyMetadata)
  , _dkrsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkrsKeyMetadata' - Metadata associated with the key.
--
-- * 'dkrsResponseStatus' - -- | The response status code.
describeKeyResponse
    :: Int -- ^ 'dkrsResponseStatus'
    -> DescribeKeyResponse
describeKeyResponse pResponseStatus_ =
  DescribeKeyResponse'
  {_dkrsKeyMetadata = Nothing, _dkrsResponseStatus = pResponseStatus_}


-- | Metadata associated with the key.
dkrsKeyMetadata :: Lens' DescribeKeyResponse (Maybe KeyMetadata)
dkrsKeyMetadata = lens _dkrsKeyMetadata (\ s a -> s{_dkrsKeyMetadata = a});

-- | -- | The response status code.
dkrsResponseStatus :: Lens' DescribeKeyResponse Int
dkrsResponseStatus = lens _dkrsResponseStatus (\ s a -> s{_dkrsResponseStatus = a});

instance NFData DescribeKeyResponse where
