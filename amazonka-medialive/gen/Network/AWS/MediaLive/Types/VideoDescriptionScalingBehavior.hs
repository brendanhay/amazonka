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
-- Module      : Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior
  ( VideoDescriptionScalingBehavior
      ( ..,
        VideoDescriptionScalingBehavior_DEFAULT,
        VideoDescriptionScalingBehavior_STRETCH_TO_OUTPUT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Video Description Scaling Behavior
newtype VideoDescriptionScalingBehavior = VideoDescriptionScalingBehavior'
  { fromVideoDescriptionScalingBehavior ::
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

pattern VideoDescriptionScalingBehavior_DEFAULT :: VideoDescriptionScalingBehavior
pattern VideoDescriptionScalingBehavior_DEFAULT = VideoDescriptionScalingBehavior' "DEFAULT"

pattern VideoDescriptionScalingBehavior_STRETCH_TO_OUTPUT :: VideoDescriptionScalingBehavior
pattern VideoDescriptionScalingBehavior_STRETCH_TO_OUTPUT = VideoDescriptionScalingBehavior' "STRETCH_TO_OUTPUT"

{-# COMPLETE
  VideoDescriptionScalingBehavior_DEFAULT,
  VideoDescriptionScalingBehavior_STRETCH_TO_OUTPUT,
  VideoDescriptionScalingBehavior'
  #-}
