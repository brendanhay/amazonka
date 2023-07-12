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
-- Module      : Amazonka.MediaConvert.Types.TtmlStylePassthrough
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.TtmlStylePassthrough
  ( TtmlStylePassthrough
      ( ..,
        TtmlStylePassthrough_DISABLED,
        TtmlStylePassthrough_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Pass through style and position information from a TTML-like input
-- source (TTML, IMSC, SMPTE-TT) to the TTML output.
newtype TtmlStylePassthrough = TtmlStylePassthrough'
  { fromTtmlStylePassthrough ::
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

pattern TtmlStylePassthrough_DISABLED :: TtmlStylePassthrough
pattern TtmlStylePassthrough_DISABLED = TtmlStylePassthrough' "DISABLED"

pattern TtmlStylePassthrough_ENABLED :: TtmlStylePassthrough
pattern TtmlStylePassthrough_ENABLED = TtmlStylePassthrough' "ENABLED"

{-# COMPLETE
  TtmlStylePassthrough_DISABLED,
  TtmlStylePassthrough_ENABLED,
  TtmlStylePassthrough'
  #-}
