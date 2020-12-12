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
        OSCGlacier,
        OSCIntelligentTiering,
        OSCReducedRedundancy,
        OSCStandard,
        OSCStandardIA
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

newtype ObjectStorageClass = ObjectStorageClass' Lude.Text
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

pattern OSCGlacier :: ObjectStorageClass
pattern OSCGlacier = ObjectStorageClass' "GLACIER"

pattern OSCIntelligentTiering :: ObjectStorageClass
pattern OSCIntelligentTiering = ObjectStorageClass' "INTELLIGENT_TIERING"

pattern OSCReducedRedundancy :: ObjectStorageClass
pattern OSCReducedRedundancy = ObjectStorageClass' "REDUCED_REDUNDANCY"

pattern OSCStandard :: ObjectStorageClass
pattern OSCStandard = ObjectStorageClass' "STANDARD"

pattern OSCStandardIA :: ObjectStorageClass
pattern OSCStandardIA = ObjectStorageClass' "STANDARD_IA"

{-# COMPLETE
  OSCGlacier,
  OSCIntelligentTiering,
  OSCReducedRedundancy,
  OSCStandard,
  OSCStandardIA,
  ObjectStorageClass'
  #-}
