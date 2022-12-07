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
-- Module      : Amazonka.MediaConvert.Types.ImscStylePassthrough
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ImscStylePassthrough
  ( ImscStylePassthrough
      ( ..,
        ImscStylePassthrough_DISABLED,
        ImscStylePassthrough_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Keep this setting enabled to have MediaConvert use the font style and
-- position information from the captions source in the output. This option
-- is available only when your input captions are IMSC, SMPTE-TT, or TTML.
-- Disable this setting for simplified output captions.
newtype ImscStylePassthrough = ImscStylePassthrough'
  { fromImscStylePassthrough ::
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

pattern ImscStylePassthrough_DISABLED :: ImscStylePassthrough
pattern ImscStylePassthrough_DISABLED = ImscStylePassthrough' "DISABLED"

pattern ImscStylePassthrough_ENABLED :: ImscStylePassthrough
pattern ImscStylePassthrough_ENABLED = ImscStylePassthrough' "ENABLED"

{-# COMPLETE
  ImscStylePassthrough_DISABLED,
  ImscStylePassthrough_ENABLED,
  ImscStylePassthrough'
  #-}
