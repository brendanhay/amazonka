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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus
  ( ActionHistoryStatus
      ( ..,
        ActionHistoryStatus_Completed,
        ActionHistoryStatus_Failed,
        ActionHistoryStatus_Unknown
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ActionHistoryStatus = ActionHistoryStatus'
  { fromActionHistoryStatus ::
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

pattern ActionHistoryStatus_Completed :: ActionHistoryStatus
pattern ActionHistoryStatus_Completed = ActionHistoryStatus' "Completed"

pattern ActionHistoryStatus_Failed :: ActionHistoryStatus
pattern ActionHistoryStatus_Failed = ActionHistoryStatus' "Failed"

pattern ActionHistoryStatus_Unknown :: ActionHistoryStatus
pattern ActionHistoryStatus_Unknown = ActionHistoryStatus' "Unknown"

{-# COMPLETE
  ActionHistoryStatus_Completed,
  ActionHistoryStatus_Failed,
  ActionHistoryStatus_Unknown,
  ActionHistoryStatus'
  #-}
