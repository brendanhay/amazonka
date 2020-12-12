{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.QuotaPeriodType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.QuotaPeriodType
  ( QuotaPeriodType
      ( QuotaPeriodType',
        Day,
        Month,
        Week
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype QuotaPeriodType = QuotaPeriodType' Lude.Text
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

pattern Day :: QuotaPeriodType
pattern Day = QuotaPeriodType' "DAY"

pattern Month :: QuotaPeriodType
pattern Month = QuotaPeriodType' "MONTH"

pattern Week :: QuotaPeriodType
pattern Week = QuotaPeriodType' "WEEK"

{-# COMPLETE
  Day,
  Month,
  Week,
  QuotaPeriodType'
  #-}
