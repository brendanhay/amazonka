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
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
  ( DolbyVisionLevel6Mode
      ( ..,
        DolbyVisionLevel6Mode_PASSTHROUGH,
        DolbyVisionLevel6Mode_RECALCULATE,
        DolbyVisionLevel6Mode_SPECIFY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision
-- MaxCLL and MaxFALL properies.
newtype DolbyVisionLevel6Mode = DolbyVisionLevel6Mode'
  { fromDolbyVisionLevel6Mode ::
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

pattern DolbyVisionLevel6Mode_PASSTHROUGH :: DolbyVisionLevel6Mode
pattern DolbyVisionLevel6Mode_PASSTHROUGH = DolbyVisionLevel6Mode' "PASSTHROUGH"

pattern DolbyVisionLevel6Mode_RECALCULATE :: DolbyVisionLevel6Mode
pattern DolbyVisionLevel6Mode_RECALCULATE = DolbyVisionLevel6Mode' "RECALCULATE"

pattern DolbyVisionLevel6Mode_SPECIFY :: DolbyVisionLevel6Mode
pattern DolbyVisionLevel6Mode_SPECIFY = DolbyVisionLevel6Mode' "SPECIFY"

{-# COMPLETE
  DolbyVisionLevel6Mode_PASSTHROUGH,
  DolbyVisionLevel6Mode_RECALCULATE,
  DolbyVisionLevel6Mode_SPECIFY,
  DolbyVisionLevel6Mode'
  #-}
