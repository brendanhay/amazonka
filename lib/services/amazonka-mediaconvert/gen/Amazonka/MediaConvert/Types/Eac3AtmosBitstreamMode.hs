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
-- Module      : Amazonka.MediaConvert.Types.Eac3AtmosBitstreamMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3AtmosBitstreamMode
  ( Eac3AtmosBitstreamMode
      ( ..,
        Eac3AtmosBitstreamMode_COMPLETE_MAIN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
-- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
newtype Eac3AtmosBitstreamMode = Eac3AtmosBitstreamMode'
  { fromEac3AtmosBitstreamMode ::
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

pattern Eac3AtmosBitstreamMode_COMPLETE_MAIN :: Eac3AtmosBitstreamMode
pattern Eac3AtmosBitstreamMode_COMPLETE_MAIN = Eac3AtmosBitstreamMode' "COMPLETE_MAIN"

{-# COMPLETE
  Eac3AtmosBitstreamMode_COMPLETE_MAIN,
  Eac3AtmosBitstreamMode'
  #-}
