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
-- Module      : Amazonka.MediaConvert.Types.Eac3PhaseControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3PhaseControl
  ( Eac3PhaseControl
      ( ..,
        Eac3PhaseControl_NO_SHIFT,
        Eac3PhaseControl_SHIFT_90_DEGREES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Controls the amount of phase-shift applied to the surround channels.
-- Only used for 3\/2 coding mode.
newtype Eac3PhaseControl = Eac3PhaseControl'
  { fromEac3PhaseControl ::
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

pattern Eac3PhaseControl_NO_SHIFT :: Eac3PhaseControl
pattern Eac3PhaseControl_NO_SHIFT = Eac3PhaseControl' "NO_SHIFT"

pattern Eac3PhaseControl_SHIFT_90_DEGREES :: Eac3PhaseControl
pattern Eac3PhaseControl_SHIFT_90_DEGREES = Eac3PhaseControl' "SHIFT_90_DEGREES"

{-# COMPLETE
  Eac3PhaseControl_NO_SHIFT,
  Eac3PhaseControl_SHIFT_90_DEGREES,
  Eac3PhaseControl'
  #-}
