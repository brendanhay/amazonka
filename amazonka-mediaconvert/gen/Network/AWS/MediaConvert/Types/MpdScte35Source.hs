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
-- Module      : Network.AWS.MediaConvert.Types.MpdScte35Source
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdScte35Source
  ( MpdScte35Source
      ( ..,
        MpdScte35Source_NONE,
        MpdScte35Source_PASSTHROUGH
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Ignore this setting unless you have SCTE-35 markers in your input video
-- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
-- appear in your input to also appear in this output. Choose None (NONE)
-- if you don\'t want those SCTE-35 markers in this output.
newtype MpdScte35Source = MpdScte35Source'
  { fromMpdScte35Source ::
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

pattern MpdScte35Source_NONE :: MpdScte35Source
pattern MpdScte35Source_NONE = MpdScte35Source' "NONE"

pattern MpdScte35Source_PASSTHROUGH :: MpdScte35Source
pattern MpdScte35Source_PASSTHROUGH = MpdScte35Source' "PASSTHROUGH"

{-# COMPLETE
  MpdScte35Source_NONE,
  MpdScte35Source_PASSTHROUGH,
  MpdScte35Source'
  #-}
