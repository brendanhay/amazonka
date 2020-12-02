{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Encryption where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ServerSideEncryption

-- | Contains the type of server-side encryption used.
--
--
--
-- /See:/ 'encryption' smart constructor.
data Encryption = Encryption'
  { _eKMSKeyId ::
      !(Maybe (Sensitive Text)),
    _eKMSContext :: !(Maybe Text),
    _eEncryptionType :: !ServerSideEncryption
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Encryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eKMSKeyId' - If the encryption type is @aws:kms@ , this optional value specifies the ID of the symmetric customer managed AWS KMS CMK to use for encryption of job results. Amazon S3 only supports symmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'eKMSContext' - If the encryption type is @aws:kms@ , this optional value can be used to specify the encryption context for the restore results.
--
-- * 'eEncryptionType' - The server-side encryption algorithm used when storing job results in Amazon S3 (for example, AES256, aws:kms).
encryption ::
  -- | 'eEncryptionType'
  ServerSideEncryption ->
  Encryption
encryption pEncryptionType_ =
  Encryption'
    { _eKMSKeyId = Nothing,
      _eKMSContext = Nothing,
      _eEncryptionType = pEncryptionType_
    }

-- | If the encryption type is @aws:kms@ , this optional value specifies the ID of the symmetric customer managed AWS KMS CMK to use for encryption of job results. Amazon S3 only supports symmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
eKMSKeyId :: Lens' Encryption (Maybe Text)
eKMSKeyId = lens _eKMSKeyId (\s a -> s {_eKMSKeyId = a}) . mapping _Sensitive

-- | If the encryption type is @aws:kms@ , this optional value can be used to specify the encryption context for the restore results.
eKMSContext :: Lens' Encryption (Maybe Text)
eKMSContext = lens _eKMSContext (\s a -> s {_eKMSContext = a})

-- | The server-side encryption algorithm used when storing job results in Amazon S3 (for example, AES256, aws:kms).
eEncryptionType :: Lens' Encryption ServerSideEncryption
eEncryptionType = lens _eEncryptionType (\s a -> s {_eEncryptionType = a})

instance Hashable Encryption

instance NFData Encryption

instance ToXML Encryption where
  toXML Encryption' {..} =
    mconcat
      [ "KMSKeyId" @= _eKMSKeyId,
        "KMSContext" @= _eKMSContext,
        "EncryptionType" @= _eEncryptionType
      ]
