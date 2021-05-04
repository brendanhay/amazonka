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
-- Module      : Network.AWS.Route53.Types.Statistic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.Statistic
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

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

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
