{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.TimeUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.TimeUnit
  ( TimeUnit
      ( TimeUnit',
        Annually,
        Daily,
        Monthly,
        Quarterly
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The time unit of the budget, such as MONTHLY or QUARTERLY.
newtype TimeUnit = TimeUnit' Lude.Text
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

pattern Annually :: TimeUnit
pattern Annually = TimeUnit' "ANNUALLY"

pattern Daily :: TimeUnit
pattern Daily = TimeUnit' "DAILY"

pattern Monthly :: TimeUnit
pattern Monthly = TimeUnit' "MONTHLY"

pattern Quarterly :: TimeUnit
pattern Quarterly = TimeUnit' "QUARTERLY"

{-# COMPLETE
  Annually,
  Daily,
  Monthly,
  Quarterly,
  TimeUnit'
  #-}
