{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageStatus
  ( PackageStatus
      ( PackageStatus',
        PSCopying,
        PSCopyFailed,
        PSValidating,
        PSValidationFailed,
        PSAvailable,
        PSDeleting,
        PSDeleted,
        PSDeleteFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PackageStatus = PackageStatus' Lude.Text
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

pattern PSCopying :: PackageStatus
pattern PSCopying = PackageStatus' "COPYING"

pattern PSCopyFailed :: PackageStatus
pattern PSCopyFailed = PackageStatus' "COPY_FAILED"

pattern PSValidating :: PackageStatus
pattern PSValidating = PackageStatus' "VALIDATING"

pattern PSValidationFailed :: PackageStatus
pattern PSValidationFailed = PackageStatus' "VALIDATION_FAILED"

pattern PSAvailable :: PackageStatus
pattern PSAvailable = PackageStatus' "AVAILABLE"

pattern PSDeleting :: PackageStatus
pattern PSDeleting = PackageStatus' "DELETING"

pattern PSDeleted :: PackageStatus
pattern PSDeleted = PackageStatus' "DELETED"

pattern PSDeleteFailed :: PackageStatus
pattern PSDeleteFailed = PackageStatus' "DELETE_FAILED"

{-# COMPLETE
  PSCopying,
  PSCopyFailed,
  PSValidating,
  PSValidationFailed,
  PSAvailable,
  PSDeleting,
  PSDeleted,
  PSDeleteFailed,
  PackageStatus'
  #-}
