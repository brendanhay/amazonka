{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainPackageStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainPackageStatus
  ( DomainPackageStatus
      ( ..,
        DomainPackageStatus_ACTIVE,
        DomainPackageStatus_ASSOCIATING,
        DomainPackageStatus_ASSOCIATION_FAILED,
        DomainPackageStatus_DISSOCIATING,
        DomainPackageStatus_DISSOCIATION_FAILED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DomainPackageStatus = DomainPackageStatus'
  { fromDomainPackageStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern DomainPackageStatus_ACTIVE :: DomainPackageStatus
pattern DomainPackageStatus_ACTIVE = DomainPackageStatus' "ACTIVE"

pattern DomainPackageStatus_ASSOCIATING :: DomainPackageStatus
pattern DomainPackageStatus_ASSOCIATING = DomainPackageStatus' "ASSOCIATING"

pattern DomainPackageStatus_ASSOCIATION_FAILED :: DomainPackageStatus
pattern DomainPackageStatus_ASSOCIATION_FAILED = DomainPackageStatus' "ASSOCIATION_FAILED"

pattern DomainPackageStatus_DISSOCIATING :: DomainPackageStatus
pattern DomainPackageStatus_DISSOCIATING = DomainPackageStatus' "DISSOCIATING"

pattern DomainPackageStatus_DISSOCIATION_FAILED :: DomainPackageStatus
pattern DomainPackageStatus_DISSOCIATION_FAILED = DomainPackageStatus' "DISSOCIATION_FAILED"

{-# COMPLETE
  DomainPackageStatus_ACTIVE,
  DomainPackageStatus_ASSOCIATING,
  DomainPackageStatus_ASSOCIATION_FAILED,
  DomainPackageStatus_DISSOCIATING,
  DomainPackageStatus_DISSOCIATION_FAILED,
  DomainPackageStatus'
  #-}
