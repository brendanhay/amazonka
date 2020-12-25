{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        DomainPackageStatusAssociating,
        DomainPackageStatusAssociationFailed,
        DomainPackageStatusActive,
        DomainPackageStatusDissociating,
        DomainPackageStatusDissociationFailed,
        fromDomainPackageStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DomainPackageStatus = DomainPackageStatus'
  { fromDomainPackageStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DomainPackageStatusAssociating :: DomainPackageStatus
pattern DomainPackageStatusAssociating = DomainPackageStatus' "ASSOCIATING"

pattern DomainPackageStatusAssociationFailed :: DomainPackageStatus
pattern DomainPackageStatusAssociationFailed = DomainPackageStatus' "ASSOCIATION_FAILED"

pattern DomainPackageStatusActive :: DomainPackageStatus
pattern DomainPackageStatusActive = DomainPackageStatus' "ACTIVE"

pattern DomainPackageStatusDissociating :: DomainPackageStatus
pattern DomainPackageStatusDissociating = DomainPackageStatus' "DISSOCIATING"

pattern DomainPackageStatusDissociationFailed :: DomainPackageStatus
pattern DomainPackageStatusDissociationFailed = DomainPackageStatus' "DISSOCIATION_FAILED"

{-# COMPLETE
  DomainPackageStatusAssociating,
  DomainPackageStatusAssociationFailed,
  DomainPackageStatusActive,
  DomainPackageStatusDissociating,
  DomainPackageStatusDissociationFailed,
  DomainPackageStatus'
  #-}
