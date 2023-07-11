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
-- Module      : Amazonka.Panorama.Types.PackageVersionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.PackageVersionStatus
  ( PackageVersionStatus
      ( ..,
        PackageVersionStatus_DELETING,
        PackageVersionStatus_FAILED,
        PackageVersionStatus_REGISTER_COMPLETED,
        PackageVersionStatus_REGISTER_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PackageVersionStatus = PackageVersionStatus'
  { fromPackageVersionStatus ::
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

pattern PackageVersionStatus_DELETING :: PackageVersionStatus
pattern PackageVersionStatus_DELETING = PackageVersionStatus' "DELETING"

pattern PackageVersionStatus_FAILED :: PackageVersionStatus
pattern PackageVersionStatus_FAILED = PackageVersionStatus' "FAILED"

pattern PackageVersionStatus_REGISTER_COMPLETED :: PackageVersionStatus
pattern PackageVersionStatus_REGISTER_COMPLETED = PackageVersionStatus' "REGISTER_COMPLETED"

pattern PackageVersionStatus_REGISTER_PENDING :: PackageVersionStatus
pattern PackageVersionStatus_REGISTER_PENDING = PackageVersionStatus' "REGISTER_PENDING"

{-# COMPLETE
  PackageVersionStatus_DELETING,
  PackageVersionStatus_FAILED,
  PackageVersionStatus_REGISTER_COMPLETED,
  PackageVersionStatus_REGISTER_PENDING,
  PackageVersionStatus'
  #-}
