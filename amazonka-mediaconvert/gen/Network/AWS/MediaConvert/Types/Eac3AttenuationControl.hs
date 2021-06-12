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
-- Module      : Network.AWS.MediaConvert.Types.Eac3AttenuationControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AttenuationControl
  ( Eac3AttenuationControl
      ( ..,
        Eac3AttenuationControl_ATTENUATE_3_DB,
        Eac3AttenuationControl_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround
-- channels. Only used for 3\/2 coding mode.
newtype Eac3AttenuationControl = Eac3AttenuationControl'
  { fromEac3AttenuationControl ::
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

pattern Eac3AttenuationControl_ATTENUATE_3_DB :: Eac3AttenuationControl
pattern Eac3AttenuationControl_ATTENUATE_3_DB = Eac3AttenuationControl' "ATTENUATE_3_DB"

pattern Eac3AttenuationControl_NONE :: Eac3AttenuationControl
pattern Eac3AttenuationControl_NONE = Eac3AttenuationControl' "NONE"

{-# COMPLETE
  Eac3AttenuationControl_ATTENUATE_3_DB,
  Eac3AttenuationControl_NONE,
  Eac3AttenuationControl'
  #-}
