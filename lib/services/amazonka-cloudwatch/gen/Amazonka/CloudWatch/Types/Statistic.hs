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
-- Module      : Amazonka.CloudWatch.Types.Statistic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.Statistic
  ( Statistic
      ( ..,
        Statistic_Average,
        Statistic_Maximum,
        Statistic_Minimum,
        Statistic_SampleCount,
        Statistic_Sum
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

pattern Statistic_Average :: Statistic
pattern Statistic_Average = Statistic' "Average"

pattern Statistic_Maximum :: Statistic
pattern Statistic_Maximum = Statistic' "Maximum"

pattern Statistic_Minimum :: Statistic
pattern Statistic_Minimum = Statistic' "Minimum"

pattern Statistic_SampleCount :: Statistic
pattern Statistic_SampleCount = Statistic' "SampleCount"

pattern Statistic_Sum :: Statistic
pattern Statistic_Sum = Statistic' "Sum"

{-# COMPLETE
  Statistic_Average,
  Statistic_Maximum,
  Statistic_Minimum,
  Statistic_SampleCount,
  Statistic_Sum,
  Statistic'
  #-}
