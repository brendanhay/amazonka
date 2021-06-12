{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
  ( MsSmoothH265PackagingType
      ( ..,
        MsSmoothH265PackagingType_HEV1,
        MsSmoothH265PackagingType_HVC1
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Ms Smooth H265 Packaging Type
newtype MsSmoothH265PackagingType = MsSmoothH265PackagingType'
  { fromMsSmoothH265PackagingType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern MsSmoothH265PackagingType_HEV1 :: MsSmoothH265PackagingType
pattern MsSmoothH265PackagingType_HEV1 = MsSmoothH265PackagingType' "HEV1"

pattern MsSmoothH265PackagingType_HVC1 :: MsSmoothH265PackagingType
pattern MsSmoothH265PackagingType_HVC1 = MsSmoothH265PackagingType' "HVC1"

{-# COMPLETE
  MsSmoothH265PackagingType_HEV1,
  MsSmoothH265PackagingType_HVC1,
  MsSmoothH265PackagingType'
  #-}
