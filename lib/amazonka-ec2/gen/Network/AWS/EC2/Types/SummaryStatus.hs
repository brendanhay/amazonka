{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SummaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SummaryStatus
  ( SummaryStatus
      ( SummaryStatus',
        SSImpaired,
        SSInitializing,
        SSInsufficientData,
        SSNotApplicable,
        SSOK
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SummaryStatus = SummaryStatus' Lude.Text
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

pattern SSImpaired :: SummaryStatus
pattern SSImpaired = SummaryStatus' "impaired"

pattern SSInitializing :: SummaryStatus
pattern SSInitializing = SummaryStatus' "initializing"

pattern SSInsufficientData :: SummaryStatus
pattern SSInsufficientData = SummaryStatus' "insufficient-data"

pattern SSNotApplicable :: SummaryStatus
pattern SSNotApplicable = SummaryStatus' "not-applicable"

pattern SSOK :: SummaryStatus
pattern SSOK = SummaryStatus' "ok"

{-# COMPLETE
  SSImpaired,
  SSInitializing,
  SSInsufficientData,
  SSNotApplicable,
  SSOK,
  SummaryStatus'
  #-}
