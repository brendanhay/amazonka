{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3AttenuationControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3AttenuationControl
  ( Eac3AttenuationControl
      ( Eac3AttenuationControl',
        Eac3AttenuationControlAttenuate3Db,
        Eac3AttenuationControlNone,
        fromEac3AttenuationControl
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Eac3 Attenuation Control
newtype Eac3AttenuationControl = Eac3AttenuationControl'
  { fromEac3AttenuationControl ::
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

pattern Eac3AttenuationControlAttenuate3Db :: Eac3AttenuationControl
pattern Eac3AttenuationControlAttenuate3Db = Eac3AttenuationControl' "ATTENUATE_3_DB"

pattern Eac3AttenuationControlNone :: Eac3AttenuationControl
pattern Eac3AttenuationControlNone = Eac3AttenuationControl' "NONE"

{-# COMPLETE
  Eac3AttenuationControlAttenuate3Db,
  Eac3AttenuationControlNone,
  Eac3AttenuationControl'
  #-}
