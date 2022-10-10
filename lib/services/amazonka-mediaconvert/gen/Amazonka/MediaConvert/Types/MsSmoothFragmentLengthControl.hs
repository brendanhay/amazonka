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
-- Module      : Amazonka.MediaConvert.Types.MsSmoothFragmentLengthControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MsSmoothFragmentLengthControl
  ( MsSmoothFragmentLengthControl
      ( ..,
        MsSmoothFragmentLengthControl_EXACT,
        MsSmoothFragmentLengthControl_GOP_MULTIPLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Specify how you want MediaConvert to determine the fragment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Fragment length (FragmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
newtype MsSmoothFragmentLengthControl = MsSmoothFragmentLengthControl'
  { fromMsSmoothFragmentLengthControl ::
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

pattern MsSmoothFragmentLengthControl_EXACT :: MsSmoothFragmentLengthControl
pattern MsSmoothFragmentLengthControl_EXACT = MsSmoothFragmentLengthControl' "EXACT"

pattern MsSmoothFragmentLengthControl_GOP_MULTIPLE :: MsSmoothFragmentLengthControl
pattern MsSmoothFragmentLengthControl_GOP_MULTIPLE = MsSmoothFragmentLengthControl' "GOP_MULTIPLE"

{-# COMPLETE
  MsSmoothFragmentLengthControl_EXACT,
  MsSmoothFragmentLengthControl_GOP_MULTIPLE,
  MsSmoothFragmentLengthControl'
  #-}
