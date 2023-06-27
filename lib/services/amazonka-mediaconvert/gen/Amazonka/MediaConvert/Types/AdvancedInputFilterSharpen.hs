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
-- Module      : Amazonka.MediaConvert.Types.AdvancedInputFilterSharpen
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AdvancedInputFilterSharpen
  ( AdvancedInputFilterSharpen
      ( ..,
        AdvancedInputFilterSharpen_HIGH,
        AdvancedInputFilterSharpen_LOW,
        AdvancedInputFilterSharpen_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optionally specify the amount of sharpening to apply when you use the
-- Advanced input filter. Sharpening adds contrast to the edges of your
-- video content and can reduce softness. To apply no sharpening: Keep the
-- default value, Off. To apply a minimal amount of sharpening choose Low,
-- or for the maximum choose High.
newtype AdvancedInputFilterSharpen = AdvancedInputFilterSharpen'
  { fromAdvancedInputFilterSharpen ::
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

pattern AdvancedInputFilterSharpen_HIGH :: AdvancedInputFilterSharpen
pattern AdvancedInputFilterSharpen_HIGH = AdvancedInputFilterSharpen' "HIGH"

pattern AdvancedInputFilterSharpen_LOW :: AdvancedInputFilterSharpen
pattern AdvancedInputFilterSharpen_LOW = AdvancedInputFilterSharpen' "LOW"

pattern AdvancedInputFilterSharpen_OFF :: AdvancedInputFilterSharpen
pattern AdvancedInputFilterSharpen_OFF = AdvancedInputFilterSharpen' "OFF"

{-# COMPLETE
  AdvancedInputFilterSharpen_HIGH,
  AdvancedInputFilterSharpen_LOW,
  AdvancedInputFilterSharpen_OFF,
  AdvancedInputFilterSharpen'
  #-}
