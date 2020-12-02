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
-- Module      : Network.AWS.Kinesis.StopStreamEncryption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables server-side encryption for a specified stream.
--
--
-- Stopping encryption is an asynchronous operation. Upon receiving the request, Kinesis Data Streams returns immediately and sets the status of the stream to @UPDATING@ . After the update is complete, Kinesis Data Streams sets the status of the stream back to @ACTIVE@ . Stopping encryption normally takes a few seconds to complete, but it can take minutes. You can continue to read and write data to your stream while its status is @UPDATING@ . Once the status of the stream is @ACTIVE@ , records written to the stream are no longer encrypted by Kinesis Data Streams.
--
-- API Limits: You can successfully disable server-side encryption 25 times in a rolling 24-hour period.
--
-- Note: It can take up to five seconds after the stream is in an @ACTIVE@ status before all records written to the stream are no longer subject to encryption. After you disabled encryption, you can verify that encryption is not applied by inspecting the API response from @PutRecord@ or @PutRecords@ .
--
module Network.AWS.Kinesis.StopStreamEncryption
    (
    -- * Creating a Request
      stopStreamEncryption
    , StopStreamEncryption
    -- * Request Lenses
    , sseStreamName
    , sseEncryptionType
    , sseKeyId

    -- * Destructuring the Response
    , stopStreamEncryptionResponse
    , StopStreamEncryptionResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopStreamEncryption' smart constructor.
data StopStreamEncryption = StopStreamEncryption'
  { _sseStreamName     :: !Text
  , _sseEncryptionType :: !EncryptionType
  , _sseKeyId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopStreamEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sseStreamName' - The name of the stream on which to stop encrypting records.
--
-- * 'sseEncryptionType' - The encryption type. The only valid value is @KMS@ .
--
-- * 'sseKeyId' - The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified Amazon Resource Name (ARN) to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
stopStreamEncryption
    :: Text -- ^ 'sseStreamName'
    -> EncryptionType -- ^ 'sseEncryptionType'
    -> Text -- ^ 'sseKeyId'
    -> StopStreamEncryption
stopStreamEncryption pStreamName_ pEncryptionType_ pKeyId_ =
  StopStreamEncryption'
    { _sseStreamName = pStreamName_
    , _sseEncryptionType = pEncryptionType_
    , _sseKeyId = pKeyId_
    }


-- | The name of the stream on which to stop encrypting records.
sseStreamName :: Lens' StopStreamEncryption Text
sseStreamName = lens _sseStreamName (\ s a -> s{_sseStreamName = a})

-- | The encryption type. The only valid value is @KMS@ .
sseEncryptionType :: Lens' StopStreamEncryption EncryptionType
sseEncryptionType = lens _sseEncryptionType (\ s a -> s{_sseEncryptionType = a})

-- | The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified Amazon Resource Name (ARN) to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
sseKeyId :: Lens' StopStreamEncryption Text
sseKeyId = lens _sseKeyId (\ s a -> s{_sseKeyId = a})

instance AWSRequest StopStreamEncryption where
        type Rs StopStreamEncryption =
             StopStreamEncryptionResponse
        request = postJSON kinesis
        response = receiveNull StopStreamEncryptionResponse'

instance Hashable StopStreamEncryption where

instance NFData StopStreamEncryption where

instance ToHeaders StopStreamEncryption where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.StopStreamEncryption" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopStreamEncryption where
        toJSON StopStreamEncryption'{..}
          = object
              (catMaybes
                 [Just ("StreamName" .= _sseStreamName),
                  Just ("EncryptionType" .= _sseEncryptionType),
                  Just ("KeyId" .= _sseKeyId)])

instance ToPath StopStreamEncryption where
        toPath = const "/"

instance ToQuery StopStreamEncryption where
        toQuery = const mempty

-- | /See:/ 'stopStreamEncryptionResponse' smart constructor.
data StopStreamEncryptionResponse =
  StopStreamEncryptionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopStreamEncryptionResponse' with the minimum fields required to make a request.
--
stopStreamEncryptionResponse
    :: StopStreamEncryptionResponse
stopStreamEncryptionResponse = StopStreamEncryptionResponse'


instance NFData StopStreamEncryptionResponse where
