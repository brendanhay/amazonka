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
-- Module      : Network.AWS.DeviceFarm.Types.InteractionMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.InteractionMode
  ( InteractionMode
      ( ..,
        InteractionMode_INTERACTIVE,
        InteractionMode_NO_VIDEO,
        InteractionMode_VIDEO_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype InteractionMode = InteractionMode'
  { fromInteractionMode ::
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

pattern InteractionMode_INTERACTIVE :: InteractionMode
pattern InteractionMode_INTERACTIVE = InteractionMode' "INTERACTIVE"

pattern InteractionMode_NO_VIDEO :: InteractionMode
pattern InteractionMode_NO_VIDEO = InteractionMode' "NO_VIDEO"

pattern InteractionMode_VIDEO_ONLY :: InteractionMode
pattern InteractionMode_VIDEO_ONLY = InteractionMode' "VIDEO_ONLY"

{-# COMPLETE
  InteractionMode_INTERACTIVE,
  InteractionMode_NO_VIDEO,
  InteractionMode_VIDEO_ONLY,
  InteractionMode'
  #-}
