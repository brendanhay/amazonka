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
-- Module      : Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
  ( H264UnregisteredSeiTimecode
      ( ..,
        H264UnregisteredSeiTimecode_DISABLED,
        H264UnregisteredSeiTimecode_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI
-- message.
newtype H264UnregisteredSeiTimecode = H264UnregisteredSeiTimecode'
  { fromH264UnregisteredSeiTimecode ::
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

pattern H264UnregisteredSeiTimecode_DISABLED :: H264UnregisteredSeiTimecode
pattern H264UnregisteredSeiTimecode_DISABLED = H264UnregisteredSeiTimecode' "DISABLED"

pattern H264UnregisteredSeiTimecode_ENABLED :: H264UnregisteredSeiTimecode
pattern H264UnregisteredSeiTimecode_ENABLED = H264UnregisteredSeiTimecode' "ENABLED"

{-# COMPLETE
  H264UnregisteredSeiTimecode_DISABLED,
  H264UnregisteredSeiTimecode_ENABLED,
  H264UnregisteredSeiTimecode'
  #-}
