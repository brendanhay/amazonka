{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.PublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.PublicKey where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a returned public key.
--
--
--
-- /See:/ 'publicKey' smart constructor.
data PublicKey = PublicKey'
  { _pkFingerprint :: !(Maybe Text),
    _pkValidityEndTime :: !(Maybe POSIX),
    _pkValue :: !(Maybe Base64),
    _pkValidityStartTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pkFingerprint' - The fingerprint of the public key.
--
-- * 'pkValidityEndTime' - The ending time of validity of the public key.
--
-- * 'pkValue' - The DER encoded public key value in PKCS#1 format.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'pkValidityStartTime' - The starting time of validity of the public key.
publicKey ::
  PublicKey
publicKey =
  PublicKey'
    { _pkFingerprint = Nothing,
      _pkValidityEndTime = Nothing,
      _pkValue = Nothing,
      _pkValidityStartTime = Nothing
    }

-- | The fingerprint of the public key.
pkFingerprint :: Lens' PublicKey (Maybe Text)
pkFingerprint = lens _pkFingerprint (\s a -> s {_pkFingerprint = a})

-- | The ending time of validity of the public key.
pkValidityEndTime :: Lens' PublicKey (Maybe UTCTime)
pkValidityEndTime = lens _pkValidityEndTime (\s a -> s {_pkValidityEndTime = a}) . mapping _Time

-- | The DER encoded public key value in PKCS#1 format.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
pkValue :: Lens' PublicKey (Maybe ByteString)
pkValue = lens _pkValue (\s a -> s {_pkValue = a}) . mapping _Base64

-- | The starting time of validity of the public key.
pkValidityStartTime :: Lens' PublicKey (Maybe UTCTime)
pkValidityStartTime = lens _pkValidityStartTime (\s a -> s {_pkValidityStartTime = a}) . mapping _Time

instance FromJSON PublicKey where
  parseJSON =
    withObject
      "PublicKey"
      ( \x ->
          PublicKey'
            <$> (x .:? "Fingerprint")
            <*> (x .:? "ValidityEndTime")
            <*> (x .:? "Value")
            <*> (x .:? "ValidityStartTime")
      )

instance Hashable PublicKey

instance NFData PublicKey
