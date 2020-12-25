{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
  ( Eac3AtmosCodingMode
      ( Eac3AtmosCodingMode',
        Eac3AtmosCodingModeCodingMode916,
        fromEac3AtmosCodingMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6 (CODING_MODE_9_1_6).
newtype Eac3AtmosCodingMode = Eac3AtmosCodingMode'
  { fromEac3AtmosCodingMode ::
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

pattern Eac3AtmosCodingModeCodingMode916 :: Eac3AtmosCodingMode
pattern Eac3AtmosCodingModeCodingMode916 = Eac3AtmosCodingMode' "CODING_MODE_9_1_6"

{-# COMPLETE
  Eac3AtmosCodingModeCodingMode916,
  Eac3AtmosCodingMode'
  #-}
