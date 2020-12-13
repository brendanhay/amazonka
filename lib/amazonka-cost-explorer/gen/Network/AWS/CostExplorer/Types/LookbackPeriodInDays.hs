{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.LookbackPeriodInDays
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.LookbackPeriodInDays
  ( LookbackPeriodInDays
      ( LookbackPeriodInDays',
        SevenDays,
        ThirtyDays,
        SixtyDays
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LookbackPeriodInDays = LookbackPeriodInDays' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern SevenDays :: LookbackPeriodInDays
pattern SevenDays = LookbackPeriodInDays' "SEVEN_DAYS"

pattern ThirtyDays :: LookbackPeriodInDays
pattern ThirtyDays = LookbackPeriodInDays' "THIRTY_DAYS"

pattern SixtyDays :: LookbackPeriodInDays
pattern SixtyDays = LookbackPeriodInDays' "SIXTY_DAYS"

{-# COMPLETE
  SevenDays,
  ThirtyDays,
  SixtyDays,
  LookbackPeriodInDays'
  #-}
