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
-- Module      : Amazonka.SageMaker.Types.FlowDefinitionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FlowDefinitionStatus
  ( FlowDefinitionStatus
      ( ..,
        FlowDefinitionStatus_Active,
        FlowDefinitionStatus_Deleting,
        FlowDefinitionStatus_Failed,
        FlowDefinitionStatus_Initializing
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FlowDefinitionStatus = FlowDefinitionStatus'
  { fromFlowDefinitionStatus ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
