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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or updates server-side encryption using an AWS KMS key for a specified stream.
--
--
-- Starting encryption is an asynchronous operation. Upon receiving the request, Amazon Kinesis returns immediately and sets the status of the stream to @UPDATING@ . After the update is complete, Amazon Kinesis sets the status of the stream back to @ACTIVE@ . Updating or applying encryption normally takes a few seconds to complete but it can take minutes. You can continue to read and write data to your stream while its status is @UPDATING@ . Once the status of the stream is @ACTIVE@ , records written to the stream will begin to be encrypted.
--
-- API Limits: You can successfully apply a new AWS KMS key for server-side encryption 25 times in a rolling 24 hour period.
--
-- Note: It can take up to 5 seconds after the stream is in an @ACTIVE@ status before all records written to the stream are encrypted. After you’ve enabled encryption, you can verify encryption was applied by inspecting the API response from @PutRecord@ or @PutRecords@ .
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
-- * 'sEncryptionType' - The encryption type to use. This parameter can be one of the following values:     * @NONE@ : Not valid for this operation. An @InvalidOperationException@ will be thrown.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed KMS key.
--
-- * 'sKeyId' - The GUID for the customer-managed KMS key to use for encryption. You can also use a Kinesis-owned master key by specifying the alias @aws/kinesis@ .
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
sStreamName = lens _sStreamName (\ s a -> s{_sStreamName = a});

-- | The encryption type to use. This parameter can be one of the following values:     * @NONE@ : Not valid for this operation. An @InvalidOperationException@ will be thrown.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed KMS key.
sEncryptionType :: Lens' StartStreamEncryption EncryptionType
sEncryptionType = lens _sEncryptionType (\ s a -> s{_sEncryptionType = a});

-- | The GUID for the customer-managed KMS key to use for encryption. You can also use a Kinesis-owned master key by specifying the alias @aws/kinesis@ .
sKeyId :: Lens' StartStreamEncryption Text
sKeyId = lens _sKeyId (\ s a -> s{_sKeyId = a});

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
