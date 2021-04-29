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

import qualified Network.AWS.Prelude as Prelude

newtype PackageStatus = PackageStatus'
  { fromPackageStatus ::
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
