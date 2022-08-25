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
-- Module      : Amazonka.LookoutEquipment.Types.StatisticalIssueStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.StatisticalIssueStatus
  ( StatisticalIssueStatus
      ( ..,
        StatisticalIssueStatus_NO_ISSUE_DETECTED,
        StatisticalIssueStatus_POTENTIAL_ISSUE_DETECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StatisticalIssueStatus = StatisticalIssueStatus'
  { fromStatisticalIssueStatus ::
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

pattern StatisticalIssueStatus_NO_ISSUE_DETECTED :: StatisticalIssueStatus
pattern StatisticalIssueStatus_NO_ISSUE_DETECTED = StatisticalIssueStatus' "NO_ISSUE_DETECTED"

pattern StatisticalIssueStatus_POTENTIAL_ISSUE_DETECTED :: StatisticalIssueStatus
pattern StatisticalIssueStatus_POTENTIAL_ISSUE_DETECTED = StatisticalIssueStatus' "POTENTIAL_ISSUE_DETECTED"

{-# COMPLETE
  StatisticalIssueStatus_NO_ISSUE_DETECTED,
  StatisticalIssueStatus_POTENTIAL_ISSUE_DETECTED,
  StatisticalIssueStatus'
  #-}
