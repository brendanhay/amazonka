{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
  ( M2tsAribCaptionsPidControl
      ( M2tsAribCaptionsPidControl',
        M2tsAribCaptionsPidControlAuto,
        M2tsAribCaptionsPidControlUseConfigured,
        fromM2tsAribCaptionsPidControl
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | M2ts Arib Captions Pid Control
newtype M2tsAribCaptionsPidControl = M2tsAribCaptionsPidControl'
  { fromM2tsAribCaptionsPidControl ::
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

pattern M2tsAribCaptionsPidControlAuto :: M2tsAribCaptionsPidControl
pattern M2tsAribCaptionsPidControlAuto = M2tsAribCaptionsPidControl' "AUTO"

pattern M2tsAribCaptionsPidControlUseConfigured :: M2tsAribCaptionsPidControl
pattern M2tsAribCaptionsPidControlUseConfigured = M2tsAribCaptionsPidControl' "USE_CONFIGURED"

{-# COMPLETE
  M2tsAribCaptionsPidControlAuto,
  M2tsAribCaptionsPidControlUseConfigured,
  M2tsAribCaptionsPidControl'
  #-}
