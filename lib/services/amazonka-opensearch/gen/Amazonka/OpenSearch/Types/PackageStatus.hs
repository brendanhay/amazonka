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
-- Module      : Amazonka.OpenSearch.Types.PackageStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.PackageStatus
  ( PackageStatus
      ( ..,
        PackageStatus_AVAILABLE,
        PackageStatus_COPYING,
        PackageStatus_COPY_FAILED,
        PackageStatus_DELETED,
        PackageStatus_DELETE_FAILED,
        PackageStatus_DELETING,
        PackageStatus_VALIDATING,
        PackageStatus_VALIDATION_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PackageStatus = PackageStatus'
  { fromPackageStatus ::
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

pattern PackageStatus_AVAILABLE :: PackageStatus
pattern PackageStatus_AVAILABLE = PackageStatus' "AVAILABLE"

pattern PackageStatus_COPYING :: PackageStatus
pattern PackageStatus_COPYING = PackageStatus' "COPYING"

pattern PackageStatus_COPY_FAILED :: PackageStatus
pattern PackageStatus_COPY_FAILED = PackageStatus' "COPY_FAILED"

pattern PackageStatus_DELETED :: PackageStatus
pattern PackageStatus_DELETED = PackageStatus' "DELETED"

pattern PackageStatus_DELETE_FAILED :: PackageStatus
pattern PackageStatus_DELETE_FAILED = PackageStatus' "DELETE_FAILED"

pattern PackageStatus_DELETING :: PackageStatus
pattern PackageStatus_DELETING = PackageStatus' "DELETING"

pattern PackageStatus_VALIDATING :: PackageStatus
pattern PackageStatus_VALIDATING = PackageStatus' "VALIDATING"

pattern PackageStatus_VALIDATION_FAILED :: PackageStatus
pattern PackageStatus_VALIDATION_FAILED = PackageStatus' "VALIDATION_FAILED"

{-# COMPLETE
  PackageStatus_AVAILABLE,
  PackageStatus_COPYING,
  PackageStatus_COPY_FAILED,
  PackageStatus_DELETED,
  PackageStatus_DELETE_FAILED,
  PackageStatus_DELETING,
  PackageStatus_VALIDATING,
  PackageStatus_VALIDATION_FAILED,
  PackageStatus'
  #-}
