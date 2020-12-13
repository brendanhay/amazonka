{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.TransitionStorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.TransitionStorageClass
  ( TransitionStorageClass
      ( TransitionStorageClass',
        TSCGlacier,
        TSCStandardIA,
        TSCOnezoneIA,
        TSCIntelligentTiering,
        TSCDeepArchive
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

newtype TransitionStorageClass = TransitionStorageClass' Lude.Text
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

pattern TSCGlacier :: TransitionStorageClass
pattern TSCGlacier = TransitionStorageClass' "GLACIER"

pattern TSCStandardIA :: TransitionStorageClass
pattern TSCStandardIA = TransitionStorageClass' "STANDARD_IA"

pattern TSCOnezoneIA :: TransitionStorageClass
pattern TSCOnezoneIA = TransitionStorageClass' "ONEZONE_IA"

pattern TSCIntelligentTiering :: TransitionStorageClass
pattern TSCIntelligentTiering = TransitionStorageClass' "INTELLIGENT_TIERING"

pattern TSCDeepArchive :: TransitionStorageClass
pattern TSCDeepArchive = TransitionStorageClass' "DEEP_ARCHIVE"

{-# COMPLETE
  TSCGlacier,
  TSCStandardIA,
  TSCOnezoneIA,
  TSCIntelligentTiering,
  TSCDeepArchive,
  TransitionStorageClass'
  #-}
