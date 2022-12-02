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
-- Module      : Amazonka.MediaLive.Types.Eac3CodingMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Eac3CodingMode
  ( Eac3CodingMode
      ( ..,
        Eac3CodingMode_CODING_MODE_1_0,
        Eac3CodingMode_CODING_MODE_2_0,
        Eac3CodingMode_CODING_MODE_3_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Eac3 Coding Mode
newtype Eac3CodingMode = Eac3CodingMode'
  { fromEac3CodingMode ::
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

pattern Eac3CodingMode_CODING_MODE_1_0 :: Eac3CodingMode
pattern Eac3CodingMode_CODING_MODE_1_0 = Eac3CodingMode' "CODING_MODE_1_0"

pattern Eac3CodingMode_CODING_MODE_2_0 :: Eac3CodingMode
pattern Eac3CodingMode_CODING_MODE_2_0 = Eac3CodingMode' "CODING_MODE_2_0"

pattern Eac3CodingMode_CODING_MODE_3_2 :: Eac3CodingMode
pattern Eac3CodingMode_CODING_MODE_3_2 = Eac3CodingMode' "CODING_MODE_3_2"

{-# COMPLETE
  Eac3CodingMode_CODING_MODE_1_0,
  Eac3CodingMode_CODING_MODE_2_0,
  Eac3CodingMode_CODING_MODE_3_2,
  Eac3CodingMode'
  #-}
