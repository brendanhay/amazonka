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
-- Module      : Amazonka.MediaLive.Types.Mp2CodingMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Mp2CodingMode
  ( Mp2CodingMode
      ( ..,
        Mp2CodingMode_CODING_MODE_1_0,
        Mp2CodingMode_CODING_MODE_2_0
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Mp2 Coding Mode
newtype Mp2CodingMode = Mp2CodingMode'
  { fromMp2CodingMode ::
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

pattern Mp2CodingMode_CODING_MODE_1_0 :: Mp2CodingMode
pattern Mp2CodingMode_CODING_MODE_1_0 = Mp2CodingMode' "CODING_MODE_1_0"

pattern Mp2CodingMode_CODING_MODE_2_0 :: Mp2CodingMode
pattern Mp2CodingMode_CODING_MODE_2_0 = Mp2CodingMode' "CODING_MODE_2_0"

{-# COMPLETE
  Mp2CodingMode_CODING_MODE_1_0,
  Mp2CodingMode_CODING_MODE_2_0,
  Mp2CodingMode'
  #-}
