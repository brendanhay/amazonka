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
-- Module      : Network.AWS.Kinesis.StartStreamEncryption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or updates server-side encryption using an AWS KMS key for a specified stream.
--
--
-- Starting encryption is an asynchronous operation. Upon receiving the request, Kinesis Data Streams returns immediately and sets the status of the stream to @UPDATING@ . After the update is complete, Kinesis Data Streams sets the status of the stream back to @ACTIVE@ . Updating or applying encryption normally takes a few seconds to complete, but it can take minutes. You can continue to read and write data to your stream while its status is @UPDATING@ . Once the status of the stream is @ACTIVE@ , encryption begins for records written to the stream.
--
-- API Limits: You can successfully apply a new AWS KMS key for server-side encryption 25 times in a rolling 24-hour period.
--
-- Note: It can take up to five seconds after the stream is in an @ACTIVE@ status before all records written to the stream are encrypted. After you enable encryption, you can verify that encryption is applied by inspecting the API response from @PutRecord@ or @PutRecords@ .
--
module Network.AWS.Kinesis.StartStreamEncryption
    (
    -- * Creating a Request
      startStreamEncryption
    , StartStreamEncryption
    -- * Request Lenses
    , sStreamName
    , sEncryptionType
    , sKeyId

    -- * Destructuring the Response
    , startStreamEncryptionResponse
    , StartStreamEncryptionResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startStreamEncryption' smart constructor.
data StartStreamEncryption = StartStreamEncryption'
  { _sStreamName     :: !Text
  , _sEncryptionType :: !EncryptionType
  , _sKeyId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartStreamEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStreamName' - The name of the stream for which to start encrypting records.
--
-- * 'sEncryptionType' - The encryption type to use. The only valid value is @KMS@ .
--
-- * 'sKeyId' - The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified Amazon Resource Name (ARN) to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
startStreamEncryption
    :: Text -- ^ 'sStreamName'
    -> EncryptionType -- ^ 'sEncryptionType'
    -> Text -- ^ 'sKeyId'
    -> StartStreamEncryption
startStreamEncryption pStreamName_ pEncryptionType_ pKeyId_ =
  StartStreamEncryption'
    { _sStreamName = pStreamName_
    , _sEncryptionType = pEncryptionType_
    , _sKeyId = pKeyId_
    }


-- | The name of the stream for which to start encrypting records.
sStreamName :: Lens' StartStreamEncryption Text
sStreamName = lens _sStreamName (\ s a -> s{_sStreamName = a})

-- | The encryption type to use. The only valid value is @KMS@ .
sEncryptionType :: Lens' StartStreamEncryption EncryptionType
sEncryptionType = lens _sEncryptionType (\ s a -> s{_sEncryptionType = a})

-- | The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified Amazon Resource Name (ARN) to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
sKeyId :: Lens' StartStreamEncryption Text
sKeyId = lens _sKeyId (\ s a -> s{_sKeyId = a})

instance AWSRequest StartStreamEncryption where
        type Rs StartStreamEncryption =
             StartStreamEncryptionResponse
        request = postJSON kinesis
        response = receiveNull StartStreamEncryptionResponse'

instance Hashable StartStreamEncryption where

instance NFData StartStreamEncryption where

instance ToHeaders StartStreamEncryption where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.StartStreamEncryption" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartStreamEncryption where
        toJSON StartStreamEncryption'{..}
          = object
              (catMaybes
                 [Just ("StreamName" .= _sStreamName),
                  Just ("EncryptionType" .= _sEncryptionType),
                  Just ("KeyId" .= _sKeyId)])

instance ToPath StartStreamEncryption where
        toPath = const "/"

instance ToQuery StartStreamEncryption where
        toQuery = const mempty

-- | /See:/ 'startStreamEncryptionResponse' smart constructor.
data StartStreamEncryptionResponse =
  StartStreamEncryptionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartStreamEncryptionResponse' with the minimum fields required to make a request.
--
startStreamEncryptionResponse
    :: StartStreamEncryptionResponse
startStreamEncryptionResponse = StartStreamEncryptionResponse'


instance NFData StartStreamEncryptionResponse where
