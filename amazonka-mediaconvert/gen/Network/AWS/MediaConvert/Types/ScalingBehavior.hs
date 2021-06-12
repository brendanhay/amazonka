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
-- Module      : Network.AWS.MediaConvert.Types.ScalingBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ScalingBehavior
  ( ScalingBehavior
      ( ..,
        ScalingBehavior_DEFAULT,
        ScalingBehavior_STRETCH_TO_OUTPUT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Specify how the service handles outputs that have a different aspect
-- ratio from the input aspect ratio. Choose Stretch to output
-- (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit.
-- Keep the setting Default (DEFAULT) to have the service letterbox your
-- video instead. This setting overrides any value that you specify for the
-- setting Selection placement (position) in this output.
newtype ScalingBehavior = ScalingBehavior'
  { fromScalingBehavior ::
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

pattern ScalingBehavior_DEFAULT :: ScalingBehavior
pattern ScalingBehavior_DEFAULT = ScalingBehavior' "DEFAULT"

pattern ScalingBehavior_STRETCH_TO_OUTPUT :: ScalingBehavior
pattern ScalingBehavior_STRETCH_TO_OUTPUT = ScalingBehavior' "STRETCH_TO_OUTPUT"

{-# COMPLETE
  ScalingBehavior_DEFAULT,
  ScalingBehavior_STRETCH_TO_OUTPUT,
  ScalingBehavior'
  #-}
