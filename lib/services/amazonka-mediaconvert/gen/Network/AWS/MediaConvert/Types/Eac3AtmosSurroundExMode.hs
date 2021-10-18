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
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
  ( Eac3AtmosSurroundExMode
      ( ..,
        Eac3AtmosSurroundExMode_DISABLED,
        Eac3AtmosSurroundExMode_ENABLED,
        Eac3AtmosSurroundExMode_NOT_INDICATED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify whether your input audio has an additional center rear surround
-- channel matrix encoded into your left and right surround channels.
newtype Eac3AtmosSurroundExMode = Eac3AtmosSurroundExMode'
  { fromEac3AtmosSurroundExMode ::
      Core.Text
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
