{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpenSearch.Types.DomainPackageStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainPackageStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DomainPackageStatus = DomainPackageStatus'
  { fromDomainPackageStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
