-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
  ( MsSmoothH265PackagingType
      ( MsSmoothH265PackagingType',
        MSHPTHEV1,
        MSHPTHVC1
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Ms Smooth H265 Packaging Type
newtype MsSmoothH265PackagingType = MsSmoothH265PackagingType' Lude.Text
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

pattern MSHPTHEV1 :: MsSmoothH265PackagingType
pattern MSHPTHEV1 = MsSmoothH265PackagingType' "HEV1"

pattern MSHPTHVC1 :: MsSmoothH265PackagingType
pattern MSHPTHVC1 = MsSmoothH265PackagingType' "HVC1"

{-# COMPLETE
  MSHPTHEV1,
  MSHPTHVC1,
  MsSmoothH265PackagingType'
  #-}
