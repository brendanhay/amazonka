-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsNielsenId3
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsNielsenId3
  ( M2tsNielsenId3
      ( M2tsNielsenId3',
        MNIInsert,
        MNINone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
newtype M2tsNielsenId3 = M2tsNielsenId3' Lude.Text
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

pattern MNIInsert :: M2tsNielsenId3
pattern MNIInsert = M2tsNielsenId3' "INSERT"

pattern MNINone :: M2tsNielsenId3
pattern MNINone = M2tsNielsenId3' "NONE"

{-# COMPLETE
  MNIInsert,
  MNINone,
  M2tsNielsenId3'
  #-}
