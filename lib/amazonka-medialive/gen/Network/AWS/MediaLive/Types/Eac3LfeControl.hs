{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3LfeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3LfeControl
  ( Eac3LfeControl
      ( Eac3LfeControl',
        Eac3LfeControlLfe,
        Eac3LfeControlNoLfe,
        fromEac3LfeControl
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Eac3 Lfe Control
newtype Eac3LfeControl = Eac3LfeControl'
  { fromEac3LfeControl ::
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

pattern Eac3LfeControlLfe :: Eac3LfeControl
pattern Eac3LfeControlLfe = Eac3LfeControl' "LFE"

pattern Eac3LfeControlNoLfe :: Eac3LfeControl
pattern Eac3LfeControlNoLfe = Eac3LfeControl' "NO_LFE"

{-# COMPLETE
  Eac3LfeControlLfe,
  Eac3LfeControlNoLfe,
  Eac3LfeControl'
  #-}
