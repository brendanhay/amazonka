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
-- Module      : Network.AWS.MediaConvert.Types.WatermarkingStrength
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.WatermarkingStrength
  ( WatermarkingStrength
      ( ..,
        WatermarkingStrength_DEFAULT,
        WatermarkingStrength_LIGHTER,
        WatermarkingStrength_LIGHTEST,
        WatermarkingStrength_STRONGER,
        WatermarkingStrength_STRONGEST
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Optional. Ignore this setting unless Nagra support directs you to
-- specify a value. When you don\'t specify a value here, the Nagra
-- NexGuard library uses its default value.
newtype WatermarkingStrength = WatermarkingStrength'
  { fromWatermarkingStrength ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern WatermarkingStrength_DEFAULT :: WatermarkingStrength
pattern WatermarkingStrength_DEFAULT = WatermarkingStrength' "DEFAULT"

pattern WatermarkingStrength_LIGHTER :: WatermarkingStrength
pattern WatermarkingStrength_LIGHTER = WatermarkingStrength' "LIGHTER"

pattern WatermarkingStrength_LIGHTEST :: WatermarkingStrength
pattern WatermarkingStrength_LIGHTEST = WatermarkingStrength' "LIGHTEST"

pattern WatermarkingStrength_STRONGER :: WatermarkingStrength
pattern WatermarkingStrength_STRONGER = WatermarkingStrength' "STRONGER"

pattern WatermarkingStrength_STRONGEST :: WatermarkingStrength
pattern WatermarkingStrength_STRONGEST = WatermarkingStrength' "STRONGEST"

{-# COMPLETE
  WatermarkingStrength_DEFAULT,
  WatermarkingStrength_LIGHTER,
  WatermarkingStrength_LIGHTEST,
  WatermarkingStrength_STRONGER,
  WatermarkingStrength_STRONGEST,
  WatermarkingStrength'
  #-}
