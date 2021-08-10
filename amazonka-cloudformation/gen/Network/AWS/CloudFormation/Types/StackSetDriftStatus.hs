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
-- Module      : Network.AWS.CloudFormation.Types.StackSetDriftStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetDriftStatus
  ( StackSetDriftStatus
      ( ..,
        StackSetDriftStatus_DRIFTED,
        StackSetDriftStatus_IN_SYNC,
        StackSetDriftStatus_NOT_CHECKED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype StackSetDriftStatus = StackSetDriftStatus'
  { fromStackSetDriftStatus ::
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

pattern StackSetDriftStatus_DRIFTED :: StackSetDriftStatus
pattern StackSetDriftStatus_DRIFTED = StackSetDriftStatus' "DRIFTED"

pattern StackSetDriftStatus_IN_SYNC :: StackSetDriftStatus
pattern StackSetDriftStatus_IN_SYNC = StackSetDriftStatus' "IN_SYNC"

pattern StackSetDriftStatus_NOT_CHECKED :: StackSetDriftStatus
pattern StackSetDriftStatus_NOT_CHECKED = StackSetDriftStatus' "NOT_CHECKED"

{-# COMPLETE
  StackSetDriftStatus_DRIFTED,
  StackSetDriftStatus_IN_SYNC,
  StackSetDriftStatus_NOT_CHECKED,
  StackSetDriftStatus'
  #-}
