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
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionStatus
  ( FlowDefinitionStatus
      ( ..,
        FlowDefinitionStatus_Active,
        FlowDefinitionStatus_Deleting,
        FlowDefinitionStatus_Failed,
        FlowDefinitionStatus_Initializing
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype FlowDefinitionStatus = FlowDefinitionStatus'
  { fromFlowDefinitionStatus ::
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

pattern FlowDefinitionStatus_Active :: FlowDefinitionStatus
pattern FlowDefinitionStatus_Active = FlowDefinitionStatus' "Active"

pattern FlowDefinitionStatus_Deleting :: FlowDefinitionStatus
pattern FlowDefinitionStatus_Deleting = FlowDefinitionStatus' "Deleting"

pattern FlowDefinitionStatus_Failed :: FlowDefinitionStatus
pattern FlowDefinitionStatus_Failed = FlowDefinitionStatus' "Failed"

pattern FlowDefinitionStatus_Initializing :: FlowDefinitionStatus
pattern FlowDefinitionStatus_Initializing = FlowDefinitionStatus' "Initializing"

{-# COMPLETE
  FlowDefinitionStatus_Active,
  FlowDefinitionStatus_Deleting,
  FlowDefinitionStatus_Failed,
  FlowDefinitionStatus_Initializing,
  FlowDefinitionStatus'
  #-}
