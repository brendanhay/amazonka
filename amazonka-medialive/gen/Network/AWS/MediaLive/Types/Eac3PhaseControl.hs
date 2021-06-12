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
-- Module      : Network.AWS.MediaLive.Types.Eac3PhaseControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3PhaseControl
  ( Eac3PhaseControl
      ( ..,
        Eac3PhaseControl_NO_SHIFT,
        Eac3PhaseControl_SHIFT_90_DEGREES
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Eac3 Phase Control
newtype Eac3PhaseControl = Eac3PhaseControl'
  { fromEac3PhaseControl ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern Eac3PhaseControl_NO_SHIFT :: Eac3PhaseControl
pattern Eac3PhaseControl_NO_SHIFT = Eac3PhaseControl' "NO_SHIFT"

pattern Eac3PhaseControl_SHIFT_90_DEGREES :: Eac3PhaseControl
pattern Eac3PhaseControl_SHIFT_90_DEGREES = Eac3PhaseControl' "SHIFT_90_DEGREES"

{-# COMPLETE
  Eac3PhaseControl_NO_SHIFT,
  Eac3PhaseControl_SHIFT_90_DEGREES,
  Eac3PhaseControl'
  #-}
