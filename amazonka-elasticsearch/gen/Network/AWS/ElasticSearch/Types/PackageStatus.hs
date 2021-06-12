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
-- Module      : Network.AWS.ElasticSearch.Types.PackageStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageStatus
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

import qualified Network.AWS.Core as Core

newtype PackageStatus = PackageStatus'
  { fromPackageStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
