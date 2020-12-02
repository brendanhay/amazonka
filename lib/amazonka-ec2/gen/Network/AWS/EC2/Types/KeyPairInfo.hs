{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.KeyPairInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.KeyPairInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a key pair.
--
--
--
-- /See:/ 'keyPairInfo' smart constructor.
data KeyPairInfo = KeyPairInfo'
  { _kpiKeyFingerprint ::
      !(Maybe Text),
    _kpiKeyName :: !(Maybe Text),
    _kpiKeyPairId :: !(Maybe Text),
    _kpiTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyPairInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpiKeyFingerprint' - If you used 'CreateKeyPair' to create the key pair, this is the SHA-1 digest of the DER encoded private key. If you used 'ImportKeyPair' to provide AWS the public key, this is the MD5 public key fingerprint as specified in section 4 of RFC4716.
--
-- * 'kpiKeyName' - The name of the key pair.
--
-- * 'kpiKeyPairId' - The ID of the key pair.
--
-- * 'kpiTags' - Any tags applied to the key pair.
keyPairInfo ::
  KeyPairInfo
keyPairInfo =
  KeyPairInfo'
    { _kpiKeyFingerprint = Nothing,
      _kpiKeyName = Nothing,
      _kpiKeyPairId = Nothing,
      _kpiTags = Nothing
    }

-- | If you used 'CreateKeyPair' to create the key pair, this is the SHA-1 digest of the DER encoded private key. If you used 'ImportKeyPair' to provide AWS the public key, this is the MD5 public key fingerprint as specified in section 4 of RFC4716.
kpiKeyFingerprint :: Lens' KeyPairInfo (Maybe Text)
kpiKeyFingerprint = lens _kpiKeyFingerprint (\s a -> s {_kpiKeyFingerprint = a})

-- | The name of the key pair.
kpiKeyName :: Lens' KeyPairInfo (Maybe Text)
kpiKeyName = lens _kpiKeyName (\s a -> s {_kpiKeyName = a})

-- | The ID of the key pair.
kpiKeyPairId :: Lens' KeyPairInfo (Maybe Text)
kpiKeyPairId = lens _kpiKeyPairId (\s a -> s {_kpiKeyPairId = a})

-- | Any tags applied to the key pair.
kpiTags :: Lens' KeyPairInfo [Tag]
kpiTags = lens _kpiTags (\s a -> s {_kpiTags = a}) . _Default . _Coerce

instance FromXML KeyPairInfo where
  parseXML x =
    KeyPairInfo'
      <$> (x .@? "keyFingerprint")
      <*> (x .@? "keyName")
      <*> (x .@? "keyPairId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable KeyPairInfo

instance NFData KeyPairInfo
