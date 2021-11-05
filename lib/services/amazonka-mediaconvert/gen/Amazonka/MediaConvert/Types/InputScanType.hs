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
-- Module      : Amazonka.MediaConvert.Types.InputScanType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.InputScanType
  ( InputScanType
      ( ..,
        InputScanType_AUTO,
        InputScanType_PSF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | When you have a progressive segmented frame (PsF) input, use this
-- setting to flag the input as PsF. MediaConvert doesn\'t automatically
-- detect PsF. Therefore, flagging your input as PsF results in better
-- preservation of video quality when you do deinterlacing and frame rate
-- conversion. If you don\'t specify, the default value is Auto (AUTO).
-- Auto is the correct setting for all inputs that are not PsF. Don\'t set
-- this value to PsF when your input is interlaced. Doing so creates
-- horizontal interlacing artifacts.
newtype InputScanType = InputScanType'
  { fromInputScanType ::
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

pattern InputScanType_AUTO :: InputScanType
pattern InputScanType_AUTO = InputScanType' "AUTO"

pattern InputScanType_PSF :: InputScanType
pattern InputScanType_PSF = InputScanType' "PSF"

{-# COMPLETE
  InputScanType_AUTO,
  InputScanType_PSF,
  InputScanType'
  #-}
