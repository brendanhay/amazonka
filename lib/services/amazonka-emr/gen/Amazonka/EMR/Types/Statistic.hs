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
-- Module      : Amazonka.EMR.Types.Statistic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.Statistic
  ( Statistic
      ( ..,
        Statistic_AVERAGE,
        Statistic_MAXIMUM,
        Statistic_MINIMUM,
        Statistic_SAMPLE_COUNT,
        Statistic_SUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Statistic = Statistic'
  { fromStatistic ::
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

pattern Statistic_AVERAGE :: Statistic
pattern Statistic_AVERAGE = Statistic' "AVERAGE"

pattern Statistic_MAXIMUM :: Statistic
pattern Statistic_MAXIMUM = Statistic' "MAXIMUM"

pattern Statistic_MINIMUM :: Statistic
pattern Statistic_MINIMUM = Statistic' "MINIMUM"

pattern Statistic_SAMPLE_COUNT :: Statistic
pattern Statistic_SAMPLE_COUNT = Statistic' "SAMPLE_COUNT"

pattern Statistic_SUM :: Statistic
pattern Statistic_SUM = Statistic' "SUM"

{-# COMPLETE
  Statistic_AVERAGE,
  Statistic_MAXIMUM,
  Statistic_MINIMUM,
  Statistic_SAMPLE_COUNT,
  Statistic_SUM,
  Statistic'
  #-}
