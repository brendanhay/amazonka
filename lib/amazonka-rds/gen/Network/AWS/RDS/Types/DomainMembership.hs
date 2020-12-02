{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DomainMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DomainMembership where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An Active Directory Domain membership record associated with the DB instance or cluster.
--
--
--
-- /See:/ 'domainMembership' smart constructor.
data DomainMembership = DomainMembership'
  { _dmStatus ::
      !(Maybe Text),
    _dmFQDN :: !(Maybe Text),
    _dmDomain :: !(Maybe Text),
    _dmIAMRoleName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmStatus' - The status of the Active Directory Domain membership for the DB instance or cluster. Values include joined, pending-join, failed, and so on.
--
-- * 'dmFQDN' - The fully qualified domain name of the Active Directory Domain.
--
-- * 'dmDomain' - The identifier of the Active Directory Domain.
--
-- * 'dmIAMRoleName' - The name of the IAM role to be used when making API calls to the Directory Service.
domainMembership ::
  DomainMembership
domainMembership =
  DomainMembership'
    { _dmStatus = Nothing,
      _dmFQDN = Nothing,
      _dmDomain = Nothing,
      _dmIAMRoleName = Nothing
    }

-- | The status of the Active Directory Domain membership for the DB instance or cluster. Values include joined, pending-join, failed, and so on.
dmStatus :: Lens' DomainMembership (Maybe Text)
dmStatus = lens _dmStatus (\s a -> s {_dmStatus = a})

-- | The fully qualified domain name of the Active Directory Domain.
dmFQDN :: Lens' DomainMembership (Maybe Text)
dmFQDN = lens _dmFQDN (\s a -> s {_dmFQDN = a})

-- | The identifier of the Active Directory Domain.
dmDomain :: Lens' DomainMembership (Maybe Text)
dmDomain = lens _dmDomain (\s a -> s {_dmDomain = a})

-- | The name of the IAM role to be used when making API calls to the Directory Service.
dmIAMRoleName :: Lens' DomainMembership (Maybe Text)
dmIAMRoleName = lens _dmIAMRoleName (\s a -> s {_dmIAMRoleName = a})

instance FromXML DomainMembership where
  parseXML x =
    DomainMembership'
      <$> (x .@? "Status")
      <*> (x .@? "FQDN")
      <*> (x .@? "Domain")
      <*> (x .@? "IAMRoleName")

instance Hashable DomainMembership

instance NFData DomainMembership
