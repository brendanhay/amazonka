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
-- Module      : Network.AWS.EMR.Types.Statistic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Statistic
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

import qualified Network.AWS.Core as Core

newtype Statistic = Statistic'
  { fromStatistic ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
