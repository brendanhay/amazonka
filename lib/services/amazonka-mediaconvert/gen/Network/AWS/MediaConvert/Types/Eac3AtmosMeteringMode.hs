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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

-- | Choose how the service meters the loudness of your audio.
newtype Eac3AtmosMeteringMode = Eac3AtmosMeteringMode'
  { fromEac3AtmosMeteringMode ::
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
