{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3LfeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3LfeFilter
  ( Eac3LfeFilter
      ( Eac3LfeFilter',
        Eac3LfeFilterEnabled,
        Eac3LfeFilterDisabled,
        fromEac3LfeFilter
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
newtype Eac3LfeFilter = Eac3LfeFilter'
  { fromEac3LfeFilter ::
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

pattern Eac3LfeFilterEnabled :: Eac3LfeFilter
pattern Eac3LfeFilterEnabled = Eac3LfeFilter' "ENABLED"

pattern Eac3LfeFilterDisabled :: Eac3LfeFilter
pattern Eac3LfeFilterDisabled = Eac3LfeFilter' "DISABLED"

{-# COMPLETE
  Eac3LfeFilterEnabled,
  Eac3LfeFilterDisabled,
  Eac3LfeFilter'
  #-}
