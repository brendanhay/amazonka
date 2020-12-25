{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosBitstreamMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosBitstreamMode
  ( Eac3AtmosBitstreamMode
      ( Eac3AtmosBitstreamMode',
        Eac3AtmosBitstreamModeCompleteMain,
        fromEac3AtmosBitstreamMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
newtype Eac3AtmosBitstreamMode = Eac3AtmosBitstreamMode'
  { fromEac3AtmosBitstreamMode ::
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

pattern Eac3AtmosBitstreamModeCompleteMain :: Eac3AtmosBitstreamMode
pattern Eac3AtmosBitstreamModeCompleteMain = Eac3AtmosBitstreamMode' "COMPLETE_MAIN"

{-# COMPLETE
  Eac3AtmosBitstreamModeCompleteMain,
  Eac3AtmosBitstreamMode'
  #-}
