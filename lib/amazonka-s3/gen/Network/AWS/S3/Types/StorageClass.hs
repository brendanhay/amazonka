{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StorageClass
  ( StorageClass
      ( StorageClass',
        StorageClassStandard,
        StorageClassReducedRedundancy,
        StorageClassStandardIa,
        StorageClassOnezoneIa,
        StorageClassIntelligentTiering,
        StorageClassGlacier,
        StorageClassDeepArchive,
        StorageClassOutposts,
        fromStorageClass
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

newtype StorageClass = StorageClass' {fromStorageClass :: Core.Text}
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

pattern StorageClassStandard :: StorageClass
pattern StorageClassStandard = StorageClass' "STANDARD"

pattern StorageClassReducedRedundancy :: StorageClass
pattern StorageClassReducedRedundancy = StorageClass' "REDUCED_REDUNDANCY"

pattern StorageClassStandardIa :: StorageClass
pattern StorageClassStandardIa = StorageClass' "STANDARD_IA"

pattern StorageClassOnezoneIa :: StorageClass
pattern StorageClassOnezoneIa = StorageClass' "ONEZONE_IA"

pattern StorageClassIntelligentTiering :: StorageClass
pattern StorageClassIntelligentTiering = StorageClass' "INTELLIGENT_TIERING"

pattern StorageClassGlacier :: StorageClass
pattern StorageClassGlacier = StorageClass' "GLACIER"

pattern StorageClassDeepArchive :: StorageClass
pattern StorageClassDeepArchive = StorageClass' "DEEP_ARCHIVE"

pattern StorageClassOutposts :: StorageClass
pattern StorageClassOutposts = StorageClass' "OUTPOSTS"

{-# COMPLETE
  StorageClassStandard,
  StorageClassReducedRedundancy,
  StorageClassStandardIa,
  StorageClassOnezoneIa,
  StorageClassIntelligentTiering,
  StorageClassGlacier,
  StorageClassDeepArchive,
  StorageClassOutposts,
  StorageClass'
  #-}
