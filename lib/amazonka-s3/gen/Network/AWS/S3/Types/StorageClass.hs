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
        DeepArchive,
        Glacier,
        IntelligentTiering,
        OnezoneIA,
        Outposts,
        ReducedRedundancy,
        Standard,
        StandardIA
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

newtype StorageClass = StorageClass' Lude.Text
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

pattern DeepArchive :: StorageClass
pattern DeepArchive = StorageClass' "DEEP_ARCHIVE"

pattern Glacier :: StorageClass
pattern Glacier = StorageClass' "GLACIER"

pattern IntelligentTiering :: StorageClass
pattern IntelligentTiering = StorageClass' "INTELLIGENT_TIERING"

pattern OnezoneIA :: StorageClass
pattern OnezoneIA = StorageClass' "ONEZONE_IA"

pattern Outposts :: StorageClass
pattern Outposts = StorageClass' "OUTPOSTS"

pattern ReducedRedundancy :: StorageClass
pattern ReducedRedundancy = StorageClass' "REDUCED_REDUNDANCY"

pattern Standard :: StorageClass
pattern Standard = StorageClass' "STANDARD"

pattern StandardIA :: StorageClass
pattern StandardIA = StorageClass' "STANDARD_IA"

{-# COMPLETE
  DeepArchive,
  Glacier,
  IntelligentTiering,
  OnezoneIA,
  Outposts,
  ReducedRedundancy,
  Standard,
  StandardIA,
  StorageClass'
  #-}
