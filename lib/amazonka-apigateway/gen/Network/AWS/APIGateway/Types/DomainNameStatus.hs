-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DomainNameStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DomainNameStatus
  ( DomainNameStatus
      ( DomainNameStatus',
        DNSAvailable,
        DNSPending,
        DNSUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DomainNameStatus = DomainNameStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern DNSAvailable :: DomainNameStatus
pattern DNSAvailable = DomainNameStatus' "AVAILABLE"

pattern DNSPending :: DomainNameStatus
pattern DNSPending = DomainNameStatus' "PENDING"

pattern DNSUpdating :: DomainNameStatus
pattern DNSUpdating = DomainNameStatus' "UPDATING"

{-# COMPLETE
  DNSAvailable,
  DNSPending,
  DNSUpdating,
  DomainNameStatus'
  #-}
