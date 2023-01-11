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
-- Module      : Amazonka.CostExplorer.Types.LookbackPeriodInDays
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.LookbackPeriodInDays
  ( LookbackPeriodInDays
      ( ..,
        LookbackPeriodInDays_SEVEN_DAYS,
        LookbackPeriodInDays_SIXTY_DAYS,
        LookbackPeriodInDays_THIRTY_DAYS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LookbackPeriodInDays = LookbackPeriodInDays'
  { fromLookbackPeriodInDays ::
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
