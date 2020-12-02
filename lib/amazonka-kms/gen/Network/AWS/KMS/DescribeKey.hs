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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides detailed information about the specified customer master key (CMK).
--
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.
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
  { _dGrantTokens :: !(Maybe [Text])
  , _dKeyId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dGrantTokens' - A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'dKeyId' - A unique identifier for the customer master key (CMK). To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with "alias/". To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
describeKey
    :: Text -- ^ 'dKeyId'
    -> DescribeKey
describeKey pKeyId_ = DescribeKey' {_dGrantTokens = Nothing, _dKeyId = pKeyId_}


-- | A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
dGrantTokens :: Lens' DescribeKey [Text]
dGrantTokens = lens _dGrantTokens (\ s a -> s{_dGrantTokens = a}) . _Default . _Coerce

-- | A unique identifier for the customer master key (CMK). To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with "alias/". To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
dKeyId :: Lens' DescribeKey Text
dKeyId = lens _dKeyId (\ s a -> s{_dKeyId = a})

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
  { _dkrsKeyMetadata    :: !(Maybe KeyMetadata)
  , _dkrsResponseStatus :: !Int
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
dkrsKeyMetadata = lens _dkrsKeyMetadata (\ s a -> s{_dkrsKeyMetadata = a})

-- | -- | The response status code.
dkrsResponseStatus :: Lens' DescribeKeyResponse Int
dkrsResponseStatus = lens _dkrsResponseStatus (\ s a -> s{_dkrsResponseStatus = a})

instance NFData DescribeKeyResponse where
