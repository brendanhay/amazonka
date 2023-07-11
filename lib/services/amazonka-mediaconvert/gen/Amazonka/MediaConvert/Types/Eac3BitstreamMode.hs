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
-- Module      : Amazonka.MediaConvert.Types.Eac3BitstreamMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3BitstreamMode
  ( Eac3BitstreamMode
      ( ..,
        Eac3BitstreamMode_COMMENTARY,
        Eac3BitstreamMode_COMPLETE_MAIN,
        Eac3BitstreamMode_EMERGENCY,
        Eac3BitstreamMode_HEARING_IMPAIRED,
        Eac3BitstreamMode_VISUALLY_IMPAIRED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
-- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
newtype Eac3BitstreamMode = Eac3BitstreamMode'
  { fromEac3BitstreamMode ::
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

pattern Eac3BitstreamMode_COMMENTARY :: Eac3BitstreamMode
pattern Eac3BitstreamMode_COMMENTARY = Eac3BitstreamMode' "COMMENTARY"

pattern Eac3BitstreamMode_COMPLETE_MAIN :: Eac3BitstreamMode
pattern Eac3BitstreamMode_COMPLETE_MAIN = Eac3BitstreamMode' "COMPLETE_MAIN"

pattern Eac3BitstreamMode_EMERGENCY :: Eac3BitstreamMode
pattern Eac3BitstreamMode_EMERGENCY = Eac3BitstreamMode' "EMERGENCY"

pattern Eac3BitstreamMode_HEARING_IMPAIRED :: Eac3BitstreamMode
pattern Eac3BitstreamMode_HEARING_IMPAIRED = Eac3BitstreamMode' "HEARING_IMPAIRED"

pattern Eac3BitstreamMode_VISUALLY_IMPAIRED :: Eac3BitstreamMode
pattern Eac3BitstreamMode_VISUALLY_IMPAIRED = Eac3BitstreamMode' "VISUALLY_IMPAIRED"

{-# COMPLETE
  Eac3BitstreamMode_COMMENTARY,
  Eac3BitstreamMode_COMPLETE_MAIN,
  Eac3BitstreamMode_EMERGENCY,
  Eac3BitstreamMode_HEARING_IMPAIRED,
  Eac3BitstreamMode_VISUALLY_IMPAIRED,
  Eac3BitstreamMode'
  #-}
