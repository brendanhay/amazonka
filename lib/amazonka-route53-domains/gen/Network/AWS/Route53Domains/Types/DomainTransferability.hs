{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainTransferability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainTransferability where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53Domains.Types.Transferable

-- | A complex type that contains information about whether the specified domain can be transferred to Route 53.
--
--
--
-- /See:/ 'domainTransferability' smart constructor.
newtype DomainTransferability = DomainTransferability'
  { _dtTransferable ::
      Maybe Transferable
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainTransferability' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTransferable' - Undocumented member.
domainTransferability ::
  DomainTransferability
domainTransferability =
  DomainTransferability' {_dtTransferable = Nothing}

-- | Undocumented member.
dtTransferable :: Lens' DomainTransferability (Maybe Transferable)
dtTransferable = lens _dtTransferable (\s a -> s {_dtTransferable = a})

instance FromJSON DomainTransferability where
  parseJSON =
    withObject
      "DomainTransferability"
      (\x -> DomainTransferability' <$> (x .:? "Transferable"))

instance Hashable DomainTransferability

instance NFData DomainTransferability
