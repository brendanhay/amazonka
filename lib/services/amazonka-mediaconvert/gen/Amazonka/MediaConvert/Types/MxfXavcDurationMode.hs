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
-- Module      : Amazonka.MediaConvert.Types.MxfXavcDurationMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MxfXavcDurationMode
  ( MxfXavcDurationMode
      ( ..,
        MxfXavcDurationMode_ALLOW_ANY_DURATION,
        MxfXavcDurationMode_DROP_FRAMES_FOR_COMPLIANCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | To create an output that complies with the XAVC file format guidelines
-- for interoperability, keep the default value, Drop frames for compliance
-- (DROP_FRAMES_FOR_COMPLIANCE). To include all frames from your input in
-- this output, keep the default setting, Allow any duration
-- (ALLOW_ANY_DURATION). The number of frames that MediaConvert excludes
-- when you set this to Drop frames for compliance depends on the output
-- frame rate and duration.
newtype MxfXavcDurationMode = MxfXavcDurationMode'
  { fromMxfXavcDurationMode ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
