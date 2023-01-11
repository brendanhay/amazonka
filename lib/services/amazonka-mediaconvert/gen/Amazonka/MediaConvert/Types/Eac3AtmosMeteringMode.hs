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
-- Module      : Amazonka.MediaConvert.Types.Eac3AtmosMeteringMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3AtmosMeteringMode
  ( Eac3AtmosMeteringMode
      ( ..,
        Eac3AtmosMeteringMode_ITU_BS_1770_1,
        Eac3AtmosMeteringMode_ITU_BS_1770_2,
        Eac3AtmosMeteringMode_ITU_BS_1770_3,
        Eac3AtmosMeteringMode_ITU_BS_1770_4,
        Eac3AtmosMeteringMode_LEQ_A
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose how the service meters the loudness of your audio.
newtype Eac3AtmosMeteringMode = Eac3AtmosMeteringMode'
  { fromEac3AtmosMeteringMode ::
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

pattern Eac3AtmosMeteringMode_ITU_BS_1770_1 :: Eac3AtmosMeteringMode
pattern Eac3AtmosMeteringMode_ITU_BS_1770_1 = Eac3AtmosMeteringMode' "ITU_BS_1770_1"

pattern Eac3AtmosMeteringMode_ITU_BS_1770_2 :: Eac3AtmosMeteringMode
pattern Eac3AtmosMeteringMode_ITU_BS_1770_2 = Eac3AtmosMeteringMode' "ITU_BS_1770_2"

pattern Eac3AtmosMeteringMode_ITU_BS_1770_3 :: Eac3AtmosMeteringMode
pattern Eac3AtmosMeteringMode_ITU_BS_1770_3 = Eac3AtmosMeteringMode' "ITU_BS_1770_3"

pattern Eac3AtmosMeteringMode_ITU_BS_1770_4 :: Eac3AtmosMeteringMode
pattern Eac3AtmosMeteringMode_ITU_BS_1770_4 = Eac3AtmosMeteringMode' "ITU_BS_1770_4"

pattern Eac3AtmosMeteringMode_LEQ_A :: Eac3AtmosMeteringMode
pattern Eac3AtmosMeteringMode_LEQ_A = Eac3AtmosMeteringMode' "LEQ_A"

{-# COMPLETE
  Eac3AtmosMeteringMode_ITU_BS_1770_1,
  Eac3AtmosMeteringMode_ITU_BS_1770_2,
  Eac3AtmosMeteringMode_ITU_BS_1770_3,
  Eac3AtmosMeteringMode_ITU_BS_1770_4,
  Eac3AtmosMeteringMode_LEQ_A,
  Eac3AtmosMeteringMode'
  #-}
