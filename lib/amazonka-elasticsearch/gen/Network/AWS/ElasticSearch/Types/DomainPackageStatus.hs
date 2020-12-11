-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainPackageStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainPackageStatus
  ( DomainPackageStatus
      ( DomainPackageStatus',
        DPSActive,
        DPSAssociating,
        DPSAssociationFailed,
        DPSDissociating,
        DPSDissociationFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DomainPackageStatus = DomainPackageStatus' Lude.Text
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

pattern DPSActive :: DomainPackageStatus
pattern DPSActive = DomainPackageStatus' "ACTIVE"

pattern DPSAssociating :: DomainPackageStatus
pattern DPSAssociating = DomainPackageStatus' "ASSOCIATING"

pattern DPSAssociationFailed :: DomainPackageStatus
pattern DPSAssociationFailed = DomainPackageStatus' "ASSOCIATION_FAILED"

pattern DPSDissociating :: DomainPackageStatus
pattern DPSDissociating = DomainPackageStatus' "DISSOCIATING"

pattern DPSDissociationFailed :: DomainPackageStatus
pattern DPSDissociationFailed = DomainPackageStatus' "DISSOCIATION_FAILED"

{-# COMPLETE
  DPSActive,
  DPSAssociating,
  DPSAssociationFailed,
  DPSDissociating,
  DPSDissociationFailed,
  DomainPackageStatus'
  #-}
