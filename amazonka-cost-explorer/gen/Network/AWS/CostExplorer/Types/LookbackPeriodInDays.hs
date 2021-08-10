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
-- Module      : Network.AWS.CostExplorer.Types.LookbackPeriodInDays
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.LookbackPeriodInDays
  ( LookbackPeriodInDays
      ( ..,
        LookbackPeriodInDays_SEVEN_DAYS,
        LookbackPeriodInDays_SIXTY_DAYS,
        LookbackPeriodInDays_THIRTY_DAYS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LookbackPeriodInDays = LookbackPeriodInDays'
  { fromLookbackPeriodInDays ::
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

pattern LookbackPeriodInDays_SEVEN_DAYS :: LookbackPeriodInDays
pattern LookbackPeriodInDays_SEVEN_DAYS = LookbackPeriodInDays' "SEVEN_DAYS"

pattern LookbackPeriodInDays_SIXTY_DAYS :: LookbackPeriodInDays
pattern LookbackPeriodInDays_SIXTY_DAYS = LookbackPeriodInDays' "SIXTY_DAYS"

pattern LookbackPeriodInDays_THIRTY_DAYS :: LookbackPeriodInDays
pattern LookbackPeriodInDays_THIRTY_DAYS = LookbackPeriodInDays' "THIRTY_DAYS"

{-# COMPLETE
  LookbackPeriodInDays_SEVEN_DAYS,
  LookbackPeriodInDays_SIXTY_DAYS,
  LookbackPeriodInDays_THIRTY_DAYS,
  LookbackPeriodInDays'
  #-}
