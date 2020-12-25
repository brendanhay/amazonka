{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3CodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3CodingMode
  ( Eac3CodingMode
      ( Eac3CodingMode',
        Eac3CodingModeCodingMode10,
        Eac3CodingModeCodingMode20,
        Eac3CodingModeCodingMode32,
        fromEac3CodingMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Dolby Digital Plus coding mode. Determines number of channels.
newtype Eac3CodingMode = Eac3CodingMode'
  { fromEac3CodingMode ::
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

pattern Eac3CodingModeCodingMode10 :: Eac3CodingMode
pattern Eac3CodingModeCodingMode10 = Eac3CodingMode' "CODING_MODE_1_0"

pattern Eac3CodingModeCodingMode20 :: Eac3CodingMode
pattern Eac3CodingModeCodingMode20 = Eac3CodingMode' "CODING_MODE_2_0"

pattern Eac3CodingModeCodingMode32 :: Eac3CodingMode
pattern Eac3CodingModeCodingMode32 = Eac3CodingMode' "CODING_MODE_3_2"

{-# COMPLETE
  Eac3CodingModeCodingMode10,
  Eac3CodingModeCodingMode20,
  Eac3CodingModeCodingMode32,
  Eac3CodingMode'
  #-}
