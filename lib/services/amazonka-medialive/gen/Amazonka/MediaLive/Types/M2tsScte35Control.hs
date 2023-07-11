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
-- Module      : Amazonka.MediaLive.Types.M2tsScte35Control
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.M2tsScte35Control
  ( M2tsScte35Control
      ( ..,
        M2tsScte35Control_NONE,
        M2tsScte35Control_PASSTHROUGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | M2ts Scte35 Control
newtype M2tsScte35Control = M2tsScte35Control'
  { fromM2tsScte35Control ::
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

pattern M2tsScte35Control_NONE :: M2tsScte35Control
pattern M2tsScte35Control_NONE = M2tsScte35Control' "NONE"

pattern M2tsScte35Control_PASSTHROUGH :: M2tsScte35Control
pattern M2tsScte35Control_PASSTHROUGH = M2tsScte35Control' "PASSTHROUGH"

{-# COMPLETE
  M2tsScte35Control_NONE,
  M2tsScte35Control_PASSTHROUGH,
  M2tsScte35Control'
  #-}
