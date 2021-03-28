{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.PackageStatus
  ( PackageStatus
    ( PackageStatus'
    , PackageStatusCopying
    , PackageStatusCopyFailed
    , PackageStatusValidating
    , PackageStatusValidationFailed
    , PackageStatusAvailable
    , PackageStatusDeleting
    , PackageStatusDeleted
    , PackageStatusDeleteFailed
    , fromPackageStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PackageStatus = PackageStatus'{fromPackageStatus ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern PackageStatusCopying :: PackageStatus
pattern PackageStatusCopying = PackageStatus' "COPYING"

pattern PackageStatusCopyFailed :: PackageStatus
pattern PackageStatusCopyFailed = PackageStatus' "COPY_FAILED"

pattern PackageStatusValidating :: PackageStatus
pattern PackageStatusValidating = PackageStatus' "VALIDATING"

pattern PackageStatusValidationFailed :: PackageStatus
pattern PackageStatusValidationFailed = PackageStatus' "VALIDATION_FAILED"

pattern PackageStatusAvailable :: PackageStatus
pattern PackageStatusAvailable = PackageStatus' "AVAILABLE"

pattern PackageStatusDeleting :: PackageStatus
pattern PackageStatusDeleting = PackageStatus' "DELETING"

pattern PackageStatusDeleted :: PackageStatus
pattern PackageStatusDeleted = PackageStatus' "DELETED"

pattern PackageStatusDeleteFailed :: PackageStatus
pattern PackageStatusDeleteFailed = PackageStatus' "DELETE_FAILED"

{-# COMPLETE 
  PackageStatusCopying,

  PackageStatusCopyFailed,

  PackageStatusValidating,

  PackageStatusValidationFailed,

  PackageStatusAvailable,

  PackageStatusDeleting,

  PackageStatusDeleted,

  PackageStatusDeleteFailed,
  PackageStatus'
  #-}
