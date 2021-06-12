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
-- Module      : Network.AWS.MediaLive.Types.AacInputType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacInputType
  ( AacInputType
      ( ..,
        AacInputType_BROADCASTER_MIXED_AD,
        AacInputType_NORMAL
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Aac Input Type
newtype AacInputType = AacInputType'
  { fromAacInputType ::
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

pattern AacInputType_BROADCASTER_MIXED_AD :: AacInputType
pattern AacInputType_BROADCASTER_MIXED_AD = AacInputType' "BROADCASTER_MIXED_AD"

pattern AacInputType_NORMAL :: AacInputType
pattern AacInputType_NORMAL = AacInputType' "NORMAL"

{-# COMPLETE
  AacInputType_BROADCASTER_MIXED_AD,
  AacInputType_NORMAL,
  AacInputType'
  #-}
