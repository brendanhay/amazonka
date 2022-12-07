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
-- Module      : Amazonka.GroundStation.Types.FrequencyUnits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.FrequencyUnits
  ( FrequencyUnits
      ( ..,
        FrequencyUnits_GHz,
        FrequencyUnits_KHz,
        FrequencyUnits_MHz
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FrequencyUnits = FrequencyUnits'
  { fromFrequencyUnits ::
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

pattern FrequencyUnits_GHz :: FrequencyUnits
pattern FrequencyUnits_GHz = FrequencyUnits' "GHz"

pattern FrequencyUnits_KHz :: FrequencyUnits
pattern FrequencyUnits_KHz = FrequencyUnits' "kHz"

pattern FrequencyUnits_MHz :: FrequencyUnits
pattern FrequencyUnits_MHz = FrequencyUnits' "MHz"

{-# COMPLETE
  FrequencyUnits_GHz,
  FrequencyUnits_KHz,
  FrequencyUnits_MHz,
  FrequencyUnits'
  #-}
