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
-- Module      : Amazonka.MediaConvert.Types.PresetListBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.PresetListBy
  ( PresetListBy
      ( ..,
        PresetListBy_CREATION_DATE,
        PresetListBy_NAME,
        PresetListBy_SYSTEM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Optional. When you request a list of presets, you can choose to list
-- them alphabetically by NAME or chronologically by CREATION_DATE. If you
-- don\'t specify, the service will list them by name.
newtype PresetListBy = PresetListBy'
  { fromPresetListBy ::
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

pattern PresetListBy_CREATION_DATE :: PresetListBy
pattern PresetListBy_CREATION_DATE = PresetListBy' "CREATION_DATE"

pattern PresetListBy_NAME :: PresetListBy
pattern PresetListBy_NAME = PresetListBy' "NAME"

pattern PresetListBy_SYSTEM :: PresetListBy
pattern PresetListBy_SYSTEM = PresetListBy' "SYSTEM"

{-# COMPLETE
  PresetListBy_CREATION_DATE,
  PresetListBy_NAME,
  PresetListBy_SYSTEM,
  PresetListBy'
  #-}
