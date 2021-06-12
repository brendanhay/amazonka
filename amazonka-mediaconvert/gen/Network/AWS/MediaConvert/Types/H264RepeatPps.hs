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
-- Module      : Network.AWS.MediaConvert.Types.H264RepeatPps
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264RepeatPps
  ( H264RepeatPps
      ( ..,
        H264RepeatPps_DISABLED,
        H264RepeatPps_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Places a PPS header on each encoded picture, even if repeated.
newtype H264RepeatPps = H264RepeatPps'
  { fromH264RepeatPps ::
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

pattern H264RepeatPps_DISABLED :: H264RepeatPps
pattern H264RepeatPps_DISABLED = H264RepeatPps' "DISABLED"

pattern H264RepeatPps_ENABLED :: H264RepeatPps
pattern H264RepeatPps_ENABLED = H264RepeatPps' "ENABLED"

{-# COMPLETE
  H264RepeatPps_DISABLED,
  H264RepeatPps_ENABLED,
  H264RepeatPps'
  #-}
