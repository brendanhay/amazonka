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
-- Module      : Amazonka.SageMaker.Types.WorkforceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.WorkforceStatus
  ( WorkforceStatus
      ( ..,
        WorkforceStatus_Active,
        WorkforceStatus_Deleting,
        WorkforceStatus_Failed,
        WorkforceStatus_Initializing,
        WorkforceStatus_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkforceStatus = WorkforceStatus'
  { fromWorkforceStatus ::
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

pattern WorkforceStatus_Active :: WorkforceStatus
pattern WorkforceStatus_Active = WorkforceStatus' "Active"

pattern WorkforceStatus_Deleting :: WorkforceStatus
pattern WorkforceStatus_Deleting = WorkforceStatus' "Deleting"

pattern WorkforceStatus_Failed :: WorkforceStatus
pattern WorkforceStatus_Failed = WorkforceStatus' "Failed"

pattern WorkforceStatus_Initializing :: WorkforceStatus
pattern WorkforceStatus_Initializing = WorkforceStatus' "Initializing"

pattern WorkforceStatus_Updating :: WorkforceStatus
pattern WorkforceStatus_Updating = WorkforceStatus' "Updating"

{-# COMPLETE
  WorkforceStatus_Active,
  WorkforceStatus_Deleting,
  WorkforceStatus_Failed,
  WorkforceStatus_Initializing,
  WorkforceStatus_Updating,
  WorkforceStatus'
  #-}
