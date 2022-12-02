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
-- Module      : Amazonka.Route53Domains.Types.OperationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.OperationStatus
  ( OperationStatus
      ( ..,
        OperationStatus_ERROR,
        OperationStatus_FAILED,
        OperationStatus_IN_PROGRESS,
        OperationStatus_SUBMITTED,
        OperationStatus_SUCCESSFUL
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

pattern OperationStatus_ERROR :: OperationStatus
pattern OperationStatus_ERROR = OperationStatus' "ERROR"

pattern OperationStatus_FAILED :: OperationStatus
pattern OperationStatus_FAILED = OperationStatus' "FAILED"

pattern OperationStatus_IN_PROGRESS :: OperationStatus
pattern OperationStatus_IN_PROGRESS = OperationStatus' "IN_PROGRESS"

pattern OperationStatus_SUBMITTED :: OperationStatus
pattern OperationStatus_SUBMITTED = OperationStatus' "SUBMITTED"

pattern OperationStatus_SUCCESSFUL :: OperationStatus
pattern OperationStatus_SUCCESSFUL = OperationStatus' "SUCCESSFUL"

{-# COMPLETE
  OperationStatus_ERROR,
  OperationStatus_FAILED,
  OperationStatus_IN_PROGRESS,
  OperationStatus_SUBMITTED,
  OperationStatus_SUCCESSFUL,
  OperationStatus'
  #-}
