{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype Statistic = Statistic'
  { fromStatistic ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
