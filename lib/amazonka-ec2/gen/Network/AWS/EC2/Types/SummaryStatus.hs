{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SummaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SummaryStatus
  ( SummaryStatus
    ( SummaryStatus'
    , SummaryStatusOK
    , SummaryStatusImpaired
    , SummaryStatusInsufficientData
    , SummaryStatusNotApplicable
    , SummaryStatusInitializing
    , fromSummaryStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SummaryStatus = SummaryStatus'{fromSummaryStatus ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern SummaryStatusOK :: SummaryStatus
pattern SummaryStatusOK = SummaryStatus' "ok"

pattern SummaryStatusImpaired :: SummaryStatus
pattern SummaryStatusImpaired = SummaryStatus' "impaired"

pattern SummaryStatusInsufficientData :: SummaryStatus
pattern SummaryStatusInsufficientData = SummaryStatus' "insufficient-data"

pattern SummaryStatusNotApplicable :: SummaryStatus
pattern SummaryStatusNotApplicable = SummaryStatus' "not-applicable"

pattern SummaryStatusInitializing :: SummaryStatus
pattern SummaryStatusInitializing = SummaryStatus' "initializing"

{-# COMPLETE 
  SummaryStatusOK,

  SummaryStatusImpaired,

  SummaryStatusInsufficientData,

  SummaryStatusNotApplicable,

  SummaryStatusInitializing,
  SummaryStatus'
  #-}
