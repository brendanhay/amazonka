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
-- Module      : Amazonka.MediaConvert.Types.Eac3AtmosSurroundExMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3AtmosSurroundExMode
  ( Eac3AtmosSurroundExMode
      ( ..,
        Eac3AtmosSurroundExMode_DISABLED,
        Eac3AtmosSurroundExMode_ENABLED,
        Eac3AtmosSurroundExMode_NOT_INDICATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether your input audio has an additional center rear surround
-- channel matrix encoded into your left and right surround channels.
newtype Eac3AtmosSurroundExMode = Eac3AtmosSurroundExMode'
  { fromEac3AtmosSurroundExMode ::
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

pattern Eac3AtmosSurroundExMode_DISABLED :: Eac3AtmosSurroundExMode
pattern Eac3AtmosSurroundExMode_DISABLED = Eac3AtmosSurroundExMode' "DISABLED"

pattern Eac3AtmosSurroundExMode_ENABLED :: Eac3AtmosSurroundExMode
pattern Eac3AtmosSurroundExMode_ENABLED = Eac3AtmosSurroundExMode' "ENABLED"

pattern Eac3AtmosSurroundExMode_NOT_INDICATED :: Eac3AtmosSurroundExMode
pattern Eac3AtmosSurroundExMode_NOT_INDICATED = Eac3AtmosSurroundExMode' "NOT_INDICATED"

{-# COMPLETE
  Eac3AtmosSurroundExMode_DISABLED,
  Eac3AtmosSurroundExMode_ENABLED,
  Eac3AtmosSurroundExMode_NOT_INDICATED,
  Eac3AtmosSurroundExMode'
  #-}
