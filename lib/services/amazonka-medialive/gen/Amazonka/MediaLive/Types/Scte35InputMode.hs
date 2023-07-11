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
-- Module      : Amazonka.MediaLive.Types.Scte35InputMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35InputMode
  ( Scte35InputMode
      ( ..,
        Scte35InputMode_FIXED,
        Scte35InputMode_FOLLOW_ACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Whether the SCTE-35 input should be the active input or a fixed input.
newtype Scte35InputMode = Scte35InputMode'
  { fromScte35InputMode ::
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

pattern Scte35InputMode_FIXED :: Scte35InputMode
pattern Scte35InputMode_FIXED = Scte35InputMode' "FIXED"

pattern Scte35InputMode_FOLLOW_ACTIVE :: Scte35InputMode
pattern Scte35InputMode_FOLLOW_ACTIVE = Scte35InputMode' "FOLLOW_ACTIVE"

{-# COMPLETE
  Scte35InputMode_FIXED,
  Scte35InputMode_FOLLOW_ACTIVE,
  Scte35InputMode'
  #-}
