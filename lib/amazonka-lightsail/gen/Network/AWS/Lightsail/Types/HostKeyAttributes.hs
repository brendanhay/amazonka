{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.HostKeyAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.HostKeyAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the public SSH host keys or the RDP certificate.
--
--
--
-- /See:/ 'hostKeyAttributes' smart constructor.
data HostKeyAttributes = HostKeyAttributes'
  { _hkaNotValidAfter ::
      !(Maybe POSIX),
    _hkaNotValidBefore :: !(Maybe POSIX),
    _hkaFingerprintSHA1 :: !(Maybe Text),
    _hkaPublicKey :: !(Maybe Text),
    _hkaAlgorithm :: !(Maybe Text),
    _hkaWitnessedAt :: !(Maybe POSIX),
    _hkaFingerprintSHA256 :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HostKeyAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hkaNotValidAfter' - The returned RDP certificate is not valid after this point in time. This value is listed only for RDP certificates.
--
-- * 'hkaNotValidBefore' - The returned RDP certificate is valid after this point in time. This value is listed only for RDP certificates.
--
-- * 'hkaFingerprintSHA1' - The SHA-1 fingerprint of the returned SSH host key or RDP certificate.     * Example of an SHA-1 SSH fingerprint: @SHA1:1CHH6FaAaXjtFOsR/t83vf91SR0@      * Example of an SHA-1 RDP fingerprint: @af:34:51:fe:09:f0:e0:da:b8:4e:56:ca:60:c2:10:ff:38:06:db:45@
--
-- * 'hkaPublicKey' - The public SSH host key or the RDP certificate.
--
-- * 'hkaAlgorithm' - The SSH host key algorithm or the RDP certificate format. For SSH host keys, the algorithm may be @ssh-rsa@ , @ecdsa-sha2-nistp256@ , @ssh-ed25519@ , etc. For RDP certificates, the algorithm is always @x509-cert@ .
--
-- * 'hkaWitnessedAt' - The time that the SSH host key or RDP certificate was recorded by Lightsail.
--
-- * 'hkaFingerprintSHA256' - The SHA-256 fingerprint of the returned SSH host key or RDP certificate.     * Example of an SHA-256 SSH fingerprint: @SHA256:KTsMnRBh1IhD17HpdfsbzeGA4jOijm5tyXsMjKVbB8o@      * Example of an SHA-256 RDP fingerprint: @03:9b:36:9f:4b:de:4e:61:70:fc:7c:c9:78:e7:d2:1a:1c:25:a8:0c:91:f6:7c:e4:d6:a0:85:c8:b4:53:99:68@
hostKeyAttributes ::
  HostKeyAttributes
hostKeyAttributes =
  HostKeyAttributes'
    { _hkaNotValidAfter = Nothing,
      _hkaNotValidBefore = Nothing,
      _hkaFingerprintSHA1 = Nothing,
      _hkaPublicKey = Nothing,
      _hkaAlgorithm = Nothing,
      _hkaWitnessedAt = Nothing,
      _hkaFingerprintSHA256 = Nothing
    }

-- | The returned RDP certificate is not valid after this point in time. This value is listed only for RDP certificates.
hkaNotValidAfter :: Lens' HostKeyAttributes (Maybe UTCTime)
hkaNotValidAfter = lens _hkaNotValidAfter (\s a -> s {_hkaNotValidAfter = a}) . mapping _Time

-- | The returned RDP certificate is valid after this point in time. This value is listed only for RDP certificates.
hkaNotValidBefore :: Lens' HostKeyAttributes (Maybe UTCTime)
hkaNotValidBefore = lens _hkaNotValidBefore (\s a -> s {_hkaNotValidBefore = a}) . mapping _Time

-- | The SHA-1 fingerprint of the returned SSH host key or RDP certificate.     * Example of an SHA-1 SSH fingerprint: @SHA1:1CHH6FaAaXjtFOsR/t83vf91SR0@      * Example of an SHA-1 RDP fingerprint: @af:34:51:fe:09:f0:e0:da:b8:4e:56:ca:60:c2:10:ff:38:06:db:45@
hkaFingerprintSHA1 :: Lens' HostKeyAttributes (Maybe Text)
hkaFingerprintSHA1 = lens _hkaFingerprintSHA1 (\s a -> s {_hkaFingerprintSHA1 = a})

-- | The public SSH host key or the RDP certificate.
hkaPublicKey :: Lens' HostKeyAttributes (Maybe Text)
hkaPublicKey = lens _hkaPublicKey (\s a -> s {_hkaPublicKey = a})

-- | The SSH host key algorithm or the RDP certificate format. For SSH host keys, the algorithm may be @ssh-rsa@ , @ecdsa-sha2-nistp256@ , @ssh-ed25519@ , etc. For RDP certificates, the algorithm is always @x509-cert@ .
hkaAlgorithm :: Lens' HostKeyAttributes (Maybe Text)
hkaAlgorithm = lens _hkaAlgorithm (\s a -> s {_hkaAlgorithm = a})

-- | The time that the SSH host key or RDP certificate was recorded by Lightsail.
hkaWitnessedAt :: Lens' HostKeyAttributes (Maybe UTCTime)
hkaWitnessedAt = lens _hkaWitnessedAt (\s a -> s {_hkaWitnessedAt = a}) . mapping _Time

-- | The SHA-256 fingerprint of the returned SSH host key or RDP certificate.     * Example of an SHA-256 SSH fingerprint: @SHA256:KTsMnRBh1IhD17HpdfsbzeGA4jOijm5tyXsMjKVbB8o@      * Example of an SHA-256 RDP fingerprint: @03:9b:36:9f:4b:de:4e:61:70:fc:7c:c9:78:e7:d2:1a:1c:25:a8:0c:91:f6:7c:e4:d6:a0:85:c8:b4:53:99:68@
hkaFingerprintSHA256 :: Lens' HostKeyAttributes (Maybe Text)
hkaFingerprintSHA256 = lens _hkaFingerprintSHA256 (\s a -> s {_hkaFingerprintSHA256 = a})

instance FromJSON HostKeyAttributes where
  parseJSON =
    withObject
      "HostKeyAttributes"
      ( \x ->
          HostKeyAttributes'
            <$> (x .:? "notValidAfter")
            <*> (x .:? "notValidBefore")
            <*> (x .:? "fingerprintSHA1")
            <*> (x .:? "publicKey")
            <*> (x .:? "algorithm")
            <*> (x .:? "witnessedAt")
            <*> (x .:? "fingerprintSHA256")
      )

instance Hashable HostKeyAttributes

instance NFData HostKeyAttributes
