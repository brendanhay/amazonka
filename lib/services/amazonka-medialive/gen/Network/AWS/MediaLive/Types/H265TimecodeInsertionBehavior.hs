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
-- Module      : Amazonka.MediaLive.Types.H265TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H265TimecodeInsertionBehavior
  ( H265TimecodeInsertionBehavior
      ( ..,
        H265TimecodeInsertionBehavior_DISABLED,
        H265TimecodeInsertionBehavior_PIC_TIMING_SEI
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | H265 Timecode Insertion Behavior
newtype H265TimecodeInsertionBehavior = H265TimecodeInsertionBehavior'
  { fromH265TimecodeInsertionBehavior ::
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

pattern H265TimecodeInsertionBehavior_DISABLED :: H265TimecodeInsertionBehavior
pattern H265TimecodeInsertionBehavior_DISABLED = H265TimecodeInsertionBehavior' "DISABLED"

pattern H265TimecodeInsertionBehavior_PIC_TIMING_SEI :: H265TimecodeInsertionBehavior
pattern H265TimecodeInsertionBehavior_PIC_TIMING_SEI = H265TimecodeInsertionBehavior' "PIC_TIMING_SEI"

{-# COMPLETE
  H265TimecodeInsertionBehavior_DISABLED,
  H265TimecodeInsertionBehavior_PIC_TIMING_SEI,
  H265TimecodeInsertionBehavior'
  #-}
