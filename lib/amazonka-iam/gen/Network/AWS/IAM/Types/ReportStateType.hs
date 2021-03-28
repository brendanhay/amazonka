{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ReportStateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.ReportStateType
  ( ReportStateType
    ( ReportStateType'
    , ReportStateTypeStarted
    , ReportStateTypeInprogress
    , ReportStateTypeComplete
    , fromReportStateType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ReportStateType = ReportStateType'{fromReportStateType ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern ReportStateTypeStarted :: ReportStateType
pattern ReportStateTypeStarted = ReportStateType' "STARTED"

pattern ReportStateTypeInprogress :: ReportStateType
pattern ReportStateTypeInprogress = ReportStateType' "INPROGRESS"

pattern ReportStateTypeComplete :: ReportStateType
pattern ReportStateTypeComplete = ReportStateType' "COMPLETE"

{-# COMPLETE 
  ReportStateTypeStarted,

  ReportStateTypeInprogress,

  ReportStateTypeComplete,
  ReportStateType'
  #-}
