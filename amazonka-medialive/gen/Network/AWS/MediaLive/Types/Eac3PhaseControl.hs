{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | Eac3 Phase Control
newtype Eac3PhaseControl = Eac3PhaseControl'
  { fromEac3PhaseControl ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
