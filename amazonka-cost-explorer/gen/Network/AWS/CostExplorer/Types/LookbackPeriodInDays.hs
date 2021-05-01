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

import qualified Network.AWS.Prelude as Prelude

newtype LookbackPeriodInDays = LookbackPeriodInDays'
  { fromLookbackPeriodInDays ::
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
