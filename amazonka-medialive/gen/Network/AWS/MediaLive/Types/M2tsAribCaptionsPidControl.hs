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
-- Module      : Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
  ( M2tsAribCaptionsPidControl
      ( ..,
        M2tsAribCaptionsPidControl_AUTO,
        M2tsAribCaptionsPidControl_USE_CONFIGURED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | M2ts Arib Captions Pid Control
newtype M2tsAribCaptionsPidControl = M2tsAribCaptionsPidControl'
  { fromM2tsAribCaptionsPidControl ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern M2tsAribCaptionsPidControl_AUTO :: M2tsAribCaptionsPidControl
pattern M2tsAribCaptionsPidControl_AUTO = M2tsAribCaptionsPidControl' "AUTO"

pattern M2tsAribCaptionsPidControl_USE_CONFIGURED :: M2tsAribCaptionsPidControl
pattern M2tsAribCaptionsPidControl_USE_CONFIGURED = M2tsAribCaptionsPidControl' "USE_CONFIGURED"

{-# COMPLETE
  M2tsAribCaptionsPidControl_AUTO,
  M2tsAribCaptionsPidControl_USE_CONFIGURED,
  M2tsAribCaptionsPidControl'
  #-}
