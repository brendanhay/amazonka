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
-- Module      : Amazonka.Lightsail.Types.OperationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.OperationStatus
  ( OperationStatus
      ( ..,
        OperationStatus_Completed,
        OperationStatus_Failed,
        OperationStatus_NotStarted,
        OperationStatus_Started,
        OperationStatus_Succeeded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OperationStatus = OperationStatus'
  { fromOperationStatus ::
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

pattern OperationStatus_Completed :: OperationStatus
pattern OperationStatus_Completed = OperationStatus' "Completed"

pattern OperationStatus_Failed :: OperationStatus
pattern OperationStatus_Failed = OperationStatus' "Failed"

pattern OperationStatus_NotStarted :: OperationStatus
pattern OperationStatus_NotStarted = OperationStatus' "NotStarted"

pattern OperationStatus_Started :: OperationStatus
pattern OperationStatus_Started = OperationStatus' "Started"

pattern OperationStatus_Succeeded :: OperationStatus
pattern OperationStatus_Succeeded = OperationStatus' "Succeeded"

{-# COMPLETE
  OperationStatus_Completed,
  OperationStatus_Failed,
  OperationStatus_NotStarted,
  OperationStatus_Started,
  OperationStatus_Succeeded,
  OperationStatus'
  #-}
