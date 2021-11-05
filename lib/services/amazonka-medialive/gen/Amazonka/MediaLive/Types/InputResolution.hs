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
-- Module      : Amazonka.MediaLive.Types.InputResolution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputResolution
  ( InputResolution
      ( ..,
        InputResolution_HD,
        InputResolution_SD,
        InputResolution_UHD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Input resolution based on lines of vertical resolution in the input; SD
-- is less than 720 lines, HD is 720 to 1080 lines, UHD is greater than
-- 1080 lines
newtype InputResolution = InputResolution'
  { fromInputResolution ::
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

pattern InputResolution_HD :: InputResolution
pattern InputResolution_HD = InputResolution' "HD"

pattern InputResolution_SD :: InputResolution
pattern InputResolution_SD = InputResolution' "SD"

pattern InputResolution_UHD :: InputResolution
pattern InputResolution_UHD = InputResolution' "UHD"

{-# COMPLETE
  InputResolution_HD,
  InputResolution_SD,
  InputResolution_UHD,
  InputResolution'
  #-}
