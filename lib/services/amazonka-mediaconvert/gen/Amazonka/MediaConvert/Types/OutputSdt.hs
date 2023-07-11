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
-- Module      : Amazonka.MediaConvert.Types.OutputSdt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.OutputSdt
  ( OutputSdt
      ( ..,
        OutputSdt_SDT_FOLLOW,
        OutputSdt_SDT_FOLLOW_IF_PRESENT,
        OutputSdt_SDT_MANUAL,
        OutputSdt_SDT_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Selects method of inserting SDT information into output stream. \"Follow
-- input SDT\" copies SDT information from input stream to output stream.
-- \"Follow input SDT if present\" copies SDT information from input stream
-- to output stream if SDT information is present in the input, otherwise
-- it will fall back on the user-defined values. Enter \"SDT Manually\"
-- means user will enter the SDT information. \"No SDT\" means output
-- stream will not contain SDT information.
newtype OutputSdt = OutputSdt'
  { fromOutputSdt ::
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

pattern OutputSdt_SDT_FOLLOW :: OutputSdt
pattern OutputSdt_SDT_FOLLOW = OutputSdt' "SDT_FOLLOW"

pattern OutputSdt_SDT_FOLLOW_IF_PRESENT :: OutputSdt
pattern OutputSdt_SDT_FOLLOW_IF_PRESENT = OutputSdt' "SDT_FOLLOW_IF_PRESENT"

pattern OutputSdt_SDT_MANUAL :: OutputSdt
pattern OutputSdt_SDT_MANUAL = OutputSdt' "SDT_MANUAL"

pattern OutputSdt_SDT_NONE :: OutputSdt
pattern OutputSdt_SDT_NONE = OutputSdt' "SDT_NONE"

{-# COMPLETE
  OutputSdt_SDT_FOLLOW,
  OutputSdt_SDT_FOLLOW_IF_PRESENT,
  OutputSdt_SDT_MANUAL,
  OutputSdt_SDT_NONE,
  OutputSdt'
  #-}
