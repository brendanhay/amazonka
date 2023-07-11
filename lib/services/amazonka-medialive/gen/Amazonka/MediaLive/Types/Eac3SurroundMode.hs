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
-- Module      : Amazonka.MediaLive.Types.Eac3SurroundMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Eac3SurroundMode
  ( Eac3SurroundMode
      ( ..,
        Eac3SurroundMode_DISABLED,
        Eac3SurroundMode_ENABLED,
        Eac3SurroundMode_NOT_INDICATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Eac3 Surround Mode
newtype Eac3SurroundMode = Eac3SurroundMode'
  { fromEac3SurroundMode ::
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

pattern Eac3SurroundMode_DISABLED :: Eac3SurroundMode
pattern Eac3SurroundMode_DISABLED = Eac3SurroundMode' "DISABLED"

pattern Eac3SurroundMode_ENABLED :: Eac3SurroundMode
pattern Eac3SurroundMode_ENABLED = Eac3SurroundMode' "ENABLED"

pattern Eac3SurroundMode_NOT_INDICATED :: Eac3SurroundMode
pattern Eac3SurroundMode_NOT_INDICATED = Eac3SurroundMode' "NOT_INDICATED"

{-# COMPLETE
  Eac3SurroundMode_DISABLED,
  Eac3SurroundMode_ENABLED,
  Eac3SurroundMode_NOT_INDICATED,
  Eac3SurroundMode'
  #-}
