{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary information about one domain.
--
--
--
-- /See:/ 'domainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { _dsExpiry :: !(Maybe POSIX),
    _dsTransferLock :: !(Maybe Bool),
    _dsAutoRenew :: !(Maybe Bool),
    _dsDomainName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsExpiry' - Expiration date of the domain in Unix time format and Coordinated Universal Time (UTC).
--
-- * 'dsTransferLock' - Indicates whether a domain is locked from unauthorized transfer to another party.
--
-- * 'dsAutoRenew' - Indicates whether the domain is automatically renewed upon expiration.
--
-- * 'dsDomainName' - The name of the domain that the summary information applies to.
domainSummary ::
  -- | 'dsDomainName'
  Text ->
  DomainSummary
domainSummary pDomainName_ =
  DomainSummary'
    { _dsExpiry = Nothing,
      _dsTransferLock = Nothing,
      _dsAutoRenew = Nothing,
      _dsDomainName = pDomainName_
    }

-- | Expiration date of the domain in Unix time format and Coordinated Universal Time (UTC).
dsExpiry :: Lens' DomainSummary (Maybe UTCTime)
dsExpiry = lens _dsExpiry (\s a -> s {_dsExpiry = a}) . mapping _Time

-- | Indicates whether a domain is locked from unauthorized transfer to another party.
dsTransferLock :: Lens' DomainSummary (Maybe Bool)
dsTransferLock = lens _dsTransferLock (\s a -> s {_dsTransferLock = a})

-- | Indicates whether the domain is automatically renewed upon expiration.
dsAutoRenew :: Lens' DomainSummary (Maybe Bool)
dsAutoRenew = lens _dsAutoRenew (\s a -> s {_dsAutoRenew = a})

-- | The name of the domain that the summary information applies to.
dsDomainName :: Lens' DomainSummary Text
dsDomainName = lens _dsDomainName (\s a -> s {_dsDomainName = a})

instance FromJSON DomainSummary where
  parseJSON =
    withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            <$> (x .:? "Expiry")
            <*> (x .:? "TransferLock")
            <*> (x .:? "AutoRenew")
            <*> (x .: "DomainName")
      )

instance Hashable DomainSummary

instance NFData DomainSummary
