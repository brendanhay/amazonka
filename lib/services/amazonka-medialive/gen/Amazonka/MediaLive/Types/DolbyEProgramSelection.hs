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
-- Module      : Amazonka.MediaLive.Types.DolbyEProgramSelection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DolbyEProgramSelection
  ( DolbyEProgramSelection
      ( ..,
        DolbyEProgramSelection_ALL_CHANNELS,
        DolbyEProgramSelection_PROGRAM_1,
        DolbyEProgramSelection_PROGRAM_2,
        DolbyEProgramSelection_PROGRAM_3,
        DolbyEProgramSelection_PROGRAM_4,
        DolbyEProgramSelection_PROGRAM_5,
        DolbyEProgramSelection_PROGRAM_6,
        DolbyEProgramSelection_PROGRAM_7,
        DolbyEProgramSelection_PROGRAM_8
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Dolby EProgram Selection
newtype DolbyEProgramSelection = DolbyEProgramSelection'
  { fromDolbyEProgramSelection ::
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

pattern DolbyEProgramSelection_ALL_CHANNELS :: DolbyEProgramSelection
pattern DolbyEProgramSelection_ALL_CHANNELS = DolbyEProgramSelection' "ALL_CHANNELS"

pattern DolbyEProgramSelection_PROGRAM_1 :: DolbyEProgramSelection
pattern DolbyEProgramSelection_PROGRAM_1 = DolbyEProgramSelection' "PROGRAM_1"

pattern DolbyEProgramSelection_PROGRAM_2 :: DolbyEProgramSelection
pattern DolbyEProgramSelection_PROGRAM_2 = DolbyEProgramSelection' "PROGRAM_2"

pattern DolbyEProgramSelection_PROGRAM_3 :: DolbyEProgramSelection
pattern DolbyEProgramSelection_PROGRAM_3 = DolbyEProgramSelection' "PROGRAM_3"

pattern DolbyEProgramSelection_PROGRAM_4 :: DolbyEProgramSelection
pattern DolbyEProgramSelection_PROGRAM_4 = DolbyEProgramSelection' "PROGRAM_4"

pattern DolbyEProgramSelection_PROGRAM_5 :: DolbyEProgramSelection
pattern DolbyEProgramSelection_PROGRAM_5 = DolbyEProgramSelection' "PROGRAM_5"

pattern DolbyEProgramSelection_PROGRAM_6 :: DolbyEProgramSelection
pattern DolbyEProgramSelection_PROGRAM_6 = DolbyEProgramSelection' "PROGRAM_6"

pattern DolbyEProgramSelection_PROGRAM_7 :: DolbyEProgramSelection
pattern DolbyEProgramSelection_PROGRAM_7 = DolbyEProgramSelection' "PROGRAM_7"

pattern DolbyEProgramSelection_PROGRAM_8 :: DolbyEProgramSelection
pattern DolbyEProgramSelection_PROGRAM_8 = DolbyEProgramSelection' "PROGRAM_8"

{-# COMPLETE
  DolbyEProgramSelection_ALL_CHANNELS,
  DolbyEProgramSelection_PROGRAM_1,
  DolbyEProgramSelection_PROGRAM_2,
  DolbyEProgramSelection_PROGRAM_3,
  DolbyEProgramSelection_PROGRAM_4,
  DolbyEProgramSelection_PROGRAM_5,
  DolbyEProgramSelection_PROGRAM_6,
  DolbyEProgramSelection_PROGRAM_7,
  DolbyEProgramSelection_PROGRAM_8,
  DolbyEProgramSelection'
  #-}
