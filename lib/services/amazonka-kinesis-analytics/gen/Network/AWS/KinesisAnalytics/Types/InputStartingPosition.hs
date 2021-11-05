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
-- Module      : Amazonka.KinesisAnalytics.Types.InputStartingPosition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.InputStartingPosition
  ( InputStartingPosition
      ( ..,
        InputStartingPosition_LAST_STOPPED_POINT,
        InputStartingPosition_NOW,
        InputStartingPosition_TRIM_HORIZON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype InputStartingPosition = InputStartingPosition'
  { fromInputStartingPosition ::
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

pattern InputStartingPosition_LAST_STOPPED_POINT :: InputStartingPosition
pattern InputStartingPosition_LAST_STOPPED_POINT = InputStartingPosition' "LAST_STOPPED_POINT"

pattern InputStartingPosition_NOW :: InputStartingPosition
pattern InputStartingPosition_NOW = InputStartingPosition' "NOW"

pattern InputStartingPosition_TRIM_HORIZON :: InputStartingPosition
pattern InputStartingPosition_TRIM_HORIZON = InputStartingPosition' "TRIM_HORIZON"

{-# COMPLETE
  InputStartingPosition_LAST_STOPPED_POINT,
  InputStartingPosition_NOW,
  InputStartingPosition_TRIM_HORIZON,
  InputStartingPosition'
  #-}
