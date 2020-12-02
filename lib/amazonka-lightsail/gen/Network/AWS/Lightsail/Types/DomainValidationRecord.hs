{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DomainValidationRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DomainValidationRecord where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.ResourceRecord
import Network.AWS.Prelude

-- | Describes the domain validation records of an Amazon Lightsail SSL/TLS certificate.
--
--
--
-- /See:/ 'domainValidationRecord' smart constructor.
data DomainValidationRecord = DomainValidationRecord'
  { _dvrResourceRecord ::
      !(Maybe ResourceRecord),
    _dvrDomainName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainValidationRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvrResourceRecord' - An object that describes the DNS records to add to your domain's DNS to validate it for the certificate.
--
-- * 'dvrDomainName' - The domain name of the certificate validation record. For example, @example.com@ or @www.example.com@ .
domainValidationRecord ::
  DomainValidationRecord
domainValidationRecord =
  DomainValidationRecord'
    { _dvrResourceRecord = Nothing,
      _dvrDomainName = Nothing
    }

-- | An object that describes the DNS records to add to your domain's DNS to validate it for the certificate.
dvrResourceRecord :: Lens' DomainValidationRecord (Maybe ResourceRecord)
dvrResourceRecord = lens _dvrResourceRecord (\s a -> s {_dvrResourceRecord = a})

-- | The domain name of the certificate validation record. For example, @example.com@ or @www.example.com@ .
dvrDomainName :: Lens' DomainValidationRecord (Maybe Text)
dvrDomainName = lens _dvrDomainName (\s a -> s {_dvrDomainName = a})

instance FromJSON DomainValidationRecord where
  parseJSON =
    withObject
      "DomainValidationRecord"
      ( \x ->
          DomainValidationRecord'
            <$> (x .:? "resourceRecord") <*> (x .:? "domainName")
      )

instance Hashable DomainValidationRecord

instance NFData DomainValidationRecord
