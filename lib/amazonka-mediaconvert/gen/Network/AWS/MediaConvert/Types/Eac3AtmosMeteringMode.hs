{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
  ( Eac3AtmosMeteringMode
      ( Eac3AtmosMeteringMode',
        Eac3AtmosMeteringModeLeqA,
        Eac3AtmosMeteringModeItuBs17701,
        Eac3AtmosMeteringModeItuBs17702,
        Eac3AtmosMeteringModeItuBs17703,
        Eac3AtmosMeteringModeItuBs17704,
        fromEac3AtmosMeteringMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Choose how the service meters the loudness of your audio.
newtype Eac3AtmosMeteringMode = Eac3AtmosMeteringMode'
  { fromEac3AtmosMeteringMode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern Eac3AtmosMeteringModeLeqA :: Eac3AtmosMeteringMode
pattern Eac3AtmosMeteringModeLeqA = Eac3AtmosMeteringMode' "LEQ_A"

pattern Eac3AtmosMeteringModeItuBs17701 :: Eac3AtmosMeteringMode
pattern Eac3AtmosMeteringModeItuBs17701 = Eac3AtmosMeteringMode' "ITU_BS_1770_1"

pattern Eac3AtmosMeteringModeItuBs17702 :: Eac3AtmosMeteringMode
pattern Eac3AtmosMeteringModeItuBs17702 = Eac3AtmosMeteringMode' "ITU_BS_1770_2"

pattern Eac3AtmosMeteringModeItuBs17703 :: Eac3AtmosMeteringMode
pattern Eac3AtmosMeteringModeItuBs17703 = Eac3AtmosMeteringMode' "ITU_BS_1770_3"

pattern Eac3AtmosMeteringModeItuBs17704 :: Eac3AtmosMeteringMode
pattern Eac3AtmosMeteringModeItuBs17704 = Eac3AtmosMeteringMode' "ITU_BS_1770_4"

{-# COMPLETE
  Eac3AtmosMeteringModeLeqA,
  Eac3AtmosMeteringModeItuBs17701,
  Eac3AtmosMeteringModeItuBs17702,
  Eac3AtmosMeteringModeItuBs17703,
  Eac3AtmosMeteringModeItuBs17704,
  Eac3AtmosMeteringMode'
  #-}
