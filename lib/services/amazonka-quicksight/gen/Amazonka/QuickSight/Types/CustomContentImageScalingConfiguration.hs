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
-- Module      : Amazonka.QuickSight.Types.CustomContentImageScalingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomContentImageScalingConfiguration
  ( CustomContentImageScalingConfiguration
      ( ..,
        CustomContentImageScalingConfiguration_DO_NOT_SCALE,
        CustomContentImageScalingConfiguration_FIT_TO_HEIGHT,
        CustomContentImageScalingConfiguration_FIT_TO_WIDTH,
        CustomContentImageScalingConfiguration_SCALE_TO_VISUAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CustomContentImageScalingConfiguration = CustomContentImageScalingConfiguration'
  { fromCustomContentImageScalingConfiguration ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern CustomContentImageScalingConfiguration_DO_NOT_SCALE :: CustomContentImageScalingConfiguration
pattern CustomContentImageScalingConfiguration_DO_NOT_SCALE = CustomContentImageScalingConfiguration' "DO_NOT_SCALE"

pattern CustomContentImageScalingConfiguration_FIT_TO_HEIGHT :: CustomContentImageScalingConfiguration
pattern CustomContentImageScalingConfiguration_FIT_TO_HEIGHT = CustomContentImageScalingConfiguration' "FIT_TO_HEIGHT"

pattern CustomContentImageScalingConfiguration_FIT_TO_WIDTH :: CustomContentImageScalingConfiguration
pattern CustomContentImageScalingConfiguration_FIT_TO_WIDTH = CustomContentImageScalingConfiguration' "FIT_TO_WIDTH"

pattern CustomContentImageScalingConfiguration_SCALE_TO_VISUAL :: CustomContentImageScalingConfiguration
pattern CustomContentImageScalingConfiguration_SCALE_TO_VISUAL = CustomContentImageScalingConfiguration' "SCALE_TO_VISUAL"

{-# COMPLETE
  CustomContentImageScalingConfiguration_DO_NOT_SCALE,
  CustomContentImageScalingConfiguration_FIT_TO_HEIGHT,
  CustomContentImageScalingConfiguration_FIT_TO_WIDTH,
  CustomContentImageScalingConfiguration_SCALE_TO_VISUAL,
  CustomContentImageScalingConfiguration'
  #-}
