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
-- Module      : Network.AWS.Panorama.Types.PackageImportJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Panorama.Types.PackageImportJobStatus
  ( PackageImportJobStatus
      ( ..,
        PackageImportJobStatus_FAILED,
        PackageImportJobStatus_PENDING,
        PackageImportJobStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PackageImportJobStatus = PackageImportJobStatus'
  { fromPackageImportJobStatus ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern PackageImportJobStatus_FAILED :: PackageImportJobStatus
pattern PackageImportJobStatus_FAILED = PackageImportJobStatus' "FAILED"

pattern PackageImportJobStatus_PENDING :: PackageImportJobStatus
pattern PackageImportJobStatus_PENDING = PackageImportJobStatus' "PENDING"

pattern PackageImportJobStatus_SUCCEEDED :: PackageImportJobStatus
pattern PackageImportJobStatus_SUCCEEDED = PackageImportJobStatus' "SUCCEEDED"

{-# COMPLETE
  PackageImportJobStatus_FAILED,
  PackageImportJobStatus_PENDING,
  PackageImportJobStatus_SUCCEEDED,
  PackageImportJobStatus'
  #-}
