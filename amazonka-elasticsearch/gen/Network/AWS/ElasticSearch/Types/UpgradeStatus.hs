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
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.UpgradeStatus
  ( UpgradeStatus
      ( ..,
        UpgradeStatus_FAILED,
        UpgradeStatus_IN_PROGRESS,
        UpgradeStatus_SUCCEEDED,
        UpgradeStatus_SUCCEEDED_WITH_ISSUES
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype UpgradeStatus = UpgradeStatus'
  { fromUpgradeStatus ::
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

pattern UpgradeStatus_FAILED :: UpgradeStatus
pattern UpgradeStatus_FAILED = UpgradeStatus' "FAILED"

pattern UpgradeStatus_IN_PROGRESS :: UpgradeStatus
pattern UpgradeStatus_IN_PROGRESS = UpgradeStatus' "IN_PROGRESS"

pattern UpgradeStatus_SUCCEEDED :: UpgradeStatus
pattern UpgradeStatus_SUCCEEDED = UpgradeStatus' "SUCCEEDED"

pattern UpgradeStatus_SUCCEEDED_WITH_ISSUES :: UpgradeStatus
pattern UpgradeStatus_SUCCEEDED_WITH_ISSUES = UpgradeStatus' "SUCCEEDED_WITH_ISSUES"

{-# COMPLETE
  UpgradeStatus_FAILED,
  UpgradeStatus_IN_PROGRESS,
  UpgradeStatus_SUCCEEDED,
  UpgradeStatus_SUCCEEDED_WITH_ISSUES,
  UpgradeStatus'
  #-}
