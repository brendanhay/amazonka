{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        MsSmoothH265PackagingTypeHEV1,
        MsSmoothH265PackagingTypeHVC1,
        fromMsSmoothH265PackagingType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Ms Smooth H265 Packaging Type
newtype MsSmoothH265PackagingType = MsSmoothH265PackagingType'
  { fromMsSmoothH265PackagingType ::
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

pattern MsSmoothH265PackagingTypeHEV1 :: MsSmoothH265PackagingType
pattern MsSmoothH265PackagingTypeHEV1 = MsSmoothH265PackagingType' "HEV1"

pattern MsSmoothH265PackagingTypeHVC1 :: MsSmoothH265PackagingType
pattern MsSmoothH265PackagingTypeHVC1 = MsSmoothH265PackagingType' "HVC1"

{-# COMPLETE
  MsSmoothH265PackagingTypeHEV1,
  MsSmoothH265PackagingTypeHVC1,
  MsSmoothH265PackagingType'
  #-}
