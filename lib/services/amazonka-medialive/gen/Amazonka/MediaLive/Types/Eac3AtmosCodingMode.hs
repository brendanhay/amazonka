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
-- Module      : Amazonka.MediaLive.Types.Eac3AtmosCodingMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Eac3AtmosCodingMode
  ( Eac3AtmosCodingMode
      ( ..,
        Eac3AtmosCodingMode_CODING_MODE_5_1_4,
        Eac3AtmosCodingMode_CODING_MODE_7_1_4,
        Eac3AtmosCodingMode_CODING_MODE_9_1_6
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Eac3 Atmos Coding Mode
newtype Eac3AtmosCodingMode = Eac3AtmosCodingMode'
  { fromEac3AtmosCodingMode ::
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

pattern Eac3AtmosCodingMode_CODING_MODE_5_1_4 :: Eac3AtmosCodingMode
pattern Eac3AtmosCodingMode_CODING_MODE_5_1_4 = Eac3AtmosCodingMode' "CODING_MODE_5_1_4"

pattern Eac3AtmosCodingMode_CODING_MODE_7_1_4 :: Eac3AtmosCodingMode
pattern Eac3AtmosCodingMode_CODING_MODE_7_1_4 = Eac3AtmosCodingMode' "CODING_MODE_7_1_4"

pattern Eac3AtmosCodingMode_CODING_MODE_9_1_6 :: Eac3AtmosCodingMode
pattern Eac3AtmosCodingMode_CODING_MODE_9_1_6 = Eac3AtmosCodingMode' "CODING_MODE_9_1_6"

{-# COMPLETE
  Eac3AtmosCodingMode_CODING_MODE_5_1_4,
  Eac3AtmosCodingMode_CODING_MODE_7_1_4,
  Eac3AtmosCodingMode_CODING_MODE_9_1_6,
  Eac3AtmosCodingMode'
  #-}
