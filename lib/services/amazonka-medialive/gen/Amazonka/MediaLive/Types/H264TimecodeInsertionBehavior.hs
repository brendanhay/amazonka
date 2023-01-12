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
-- Module      : Amazonka.MediaLive.Types.H264TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H264TimecodeInsertionBehavior
  ( H264TimecodeInsertionBehavior
      ( ..,
        H264TimecodeInsertionBehavior_DISABLED,
        H264TimecodeInsertionBehavior_PIC_TIMING_SEI
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H264 Timecode Insertion Behavior
newtype H264TimecodeInsertionBehavior = H264TimecodeInsertionBehavior'
  { fromH264TimecodeInsertionBehavior ::
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

pattern H264TimecodeInsertionBehavior_DISABLED :: H264TimecodeInsertionBehavior
pattern H264TimecodeInsertionBehavior_DISABLED = H264TimecodeInsertionBehavior' "DISABLED"

pattern H264TimecodeInsertionBehavior_PIC_TIMING_SEI :: H264TimecodeInsertionBehavior
pattern H264TimecodeInsertionBehavior_PIC_TIMING_SEI = H264TimecodeInsertionBehavior' "PIC_TIMING_SEI"

{-# COMPLETE
  H264TimecodeInsertionBehavior_DISABLED,
  H264TimecodeInsertionBehavior_PIC_TIMING_SEI,
  H264TimecodeInsertionBehavior'
  #-}
