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
-- Module      : Amazonka.MediaConvert.Types.DolbyVisionLevel6Mode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DolbyVisionLevel6Mode
  ( DolbyVisionLevel6Mode
      ( ..,
        DolbyVisionLevel6Mode_PASSTHROUGH,
        DolbyVisionLevel6Mode_RECALCULATE,
        DolbyVisionLevel6Mode_SPECIFY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision
-- MaxCLL and MaxFALL properies.
newtype DolbyVisionLevel6Mode = DolbyVisionLevel6Mode'
  { fromDolbyVisionLevel6Mode ::
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

pattern DolbyVisionLevel6Mode_PASSTHROUGH :: DolbyVisionLevel6Mode
pattern DolbyVisionLevel6Mode_PASSTHROUGH = DolbyVisionLevel6Mode' "PASSTHROUGH"

pattern DolbyVisionLevel6Mode_RECALCULATE :: DolbyVisionLevel6Mode
pattern DolbyVisionLevel6Mode_RECALCULATE = DolbyVisionLevel6Mode' "RECALCULATE"

pattern DolbyVisionLevel6Mode_SPECIFY :: DolbyVisionLevel6Mode
pattern DolbyVisionLevel6Mode_SPECIFY = DolbyVisionLevel6Mode' "SPECIFY"

{-# COMPLETE
  DolbyVisionLevel6Mode_PASSTHROUGH,
  DolbyVisionLevel6Mode_RECALCULATE,
  DolbyVisionLevel6Mode_SPECIFY,
  DolbyVisionLevel6Mode'
  #-}
