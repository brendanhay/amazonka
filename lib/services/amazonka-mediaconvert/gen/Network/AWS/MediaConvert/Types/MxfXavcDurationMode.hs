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
-- Module      : Network.AWS.MediaConvert.Types.MxfXavcDurationMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MxfXavcDurationMode
  ( MxfXavcDurationMode
      ( ..,
        MxfXavcDurationMode_ALLOW_ANY_DURATION,
        MxfXavcDurationMode_DROP_FRAMES_FOR_COMPLIANCE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | To create an output that complies with the XAVC file format guidelines
-- for interoperability, keep the default value, Drop frames for compliance
-- (DROP_FRAMES_FOR_COMPLIANCE). To include all frames from your input in
-- this output, keep the default setting, Allow any duration
-- (ALLOW_ANY_DURATION). The number of frames that MediaConvert excludes
-- when you set this to Drop frames for compliance depends on the output
-- frame rate and duration.
newtype MxfXavcDurationMode = MxfXavcDurationMode'
  { fromMxfXavcDurationMode ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern MxfXavcDurationMode_ALLOW_ANY_DURATION :: MxfXavcDurationMode
pattern MxfXavcDurationMode_ALLOW_ANY_DURATION = MxfXavcDurationMode' "ALLOW_ANY_DURATION"

pattern MxfXavcDurationMode_DROP_FRAMES_FOR_COMPLIANCE :: MxfXavcDurationMode
pattern MxfXavcDurationMode_DROP_FRAMES_FOR_COMPLIANCE = MxfXavcDurationMode' "DROP_FRAMES_FOR_COMPLIANCE"

{-# COMPLETE
  MxfXavcDurationMode_ALLOW_ANY_DURATION,
  MxfXavcDurationMode_DROP_FRAMES_FOR_COMPLIANCE,
  MxfXavcDurationMode'
  #-}
