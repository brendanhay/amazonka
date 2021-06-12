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
-- Module      : Network.AWS.Connect.Types.Statistic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Statistic
  ( Statistic
      ( ..,
        Statistic_AVG,
        Statistic_MAX,
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

pattern Statistic_AVG :: Statistic
pattern Statistic_AVG = Statistic' "AVG"

pattern Statistic_MAX :: Statistic
pattern Statistic_MAX = Statistic' "MAX"

pattern Statistic_SUM :: Statistic
pattern Statistic_SUM = Statistic' "SUM"

{-# COMPLETE
  Statistic_AVG,
  Statistic_MAX,
  Statistic_SUM,
  Statistic'
  #-}
