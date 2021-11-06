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
-- Module      : Amazonka.LookoutMetrics.Types.Frequency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.Frequency
  ( Frequency
      ( ..,
        Frequency_P1D,
        Frequency_PT10M,
        Frequency_PT1H,
        Frequency_PT5M
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype Frequency = Frequency'
  { fromFrequency ::
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

pattern Frequency_P1D :: Frequency
pattern Frequency_P1D = Frequency' "P1D"

pattern Frequency_PT10M :: Frequency
pattern Frequency_PT10M = Frequency' "PT10M"

pattern Frequency_PT1H :: Frequency
pattern Frequency_PT1H = Frequency' "PT1H"

pattern Frequency_PT5M :: Frequency
pattern Frequency_PT5M = Frequency' "PT5M"

{-# COMPLETE
  Frequency_P1D,
  Frequency_PT10M,
  Frequency_PT1H,
  Frequency_PT5M,
  Frequency'
  #-}
