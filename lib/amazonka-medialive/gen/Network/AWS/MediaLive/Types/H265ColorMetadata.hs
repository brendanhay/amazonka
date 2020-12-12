{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265ColorMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265ColorMetadata
  ( H265ColorMetadata
      ( H265ColorMetadata',
        HCMIgnore,
        HCMInsert
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H265 Color Metadata
newtype H265ColorMetadata = H265ColorMetadata' Lude.Text
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

pattern HCMIgnore :: H265ColorMetadata
pattern HCMIgnore = H265ColorMetadata' "IGNORE"

pattern HCMInsert :: H265ColorMetadata
pattern HCMInsert = H265ColorMetadata' "INSERT"

{-# COMPLETE
  HCMIgnore,
  HCMInsert,
  H265ColorMetadata'
  #-}
