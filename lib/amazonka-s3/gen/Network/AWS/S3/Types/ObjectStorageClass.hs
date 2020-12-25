{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectStorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectStorageClass
  ( ObjectStorageClass
      ( ObjectStorageClass',
        ObjectStorageClassStandard,
        ObjectStorageClassStandardIa,
        ObjectStorageClassReducedRedundancy,
        ObjectStorageClassGlacier,
        ObjectStorageClassIntelligentTiering,
        fromObjectStorageClass
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

newtype ObjectStorageClass = ObjectStorageClass'
  { fromObjectStorageClass ::
      Core.Text
  }
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

pattern ObjectStorageClassStandard :: ObjectStorageClass
pattern ObjectStorageClassStandard = ObjectStorageClass' "STANDARD"

pattern ObjectStorageClassStandardIa :: ObjectStorageClass
pattern ObjectStorageClassStandardIa = ObjectStorageClass' "STANDARD_IA"

pattern ObjectStorageClassReducedRedundancy :: ObjectStorageClass
pattern ObjectStorageClassReducedRedundancy = ObjectStorageClass' "REDUCED_REDUNDANCY"

pattern ObjectStorageClassGlacier :: ObjectStorageClass
pattern ObjectStorageClassGlacier = ObjectStorageClass' "GLACIER"

pattern ObjectStorageClassIntelligentTiering :: ObjectStorageClass
pattern ObjectStorageClassIntelligentTiering = ObjectStorageClass' "INTELLIGENT_TIERING"

{-# COMPLETE
  ObjectStorageClassStandard,
  ObjectStorageClassStandardIa,
  ObjectStorageClassReducedRedundancy,
  ObjectStorageClassGlacier,
  ObjectStorageClassIntelligentTiering,
  ObjectStorageClass'
  #-}
