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
-- Module      : Network.AWS.CloudFormation.Types.OperationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.OperationStatus
  ( OperationStatus
      ( ..,
        OperationStatus_FAILED,
        OperationStatus_IN_PROGRESS,
        OperationStatus_PENDING,
        OperationStatus_SUCCESS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype OperationStatus = OperationStatus'
  { fromOperationStatus ::
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

pattern OperationStatus_FAILED :: OperationStatus
pattern OperationStatus_FAILED = OperationStatus' "FAILED"

pattern OperationStatus_IN_PROGRESS :: OperationStatus
pattern OperationStatus_IN_PROGRESS = OperationStatus' "IN_PROGRESS"

pattern OperationStatus_PENDING :: OperationStatus
pattern OperationStatus_PENDING = OperationStatus' "PENDING"

pattern OperationStatus_SUCCESS :: OperationStatus
pattern OperationStatus_SUCCESS = OperationStatus' "SUCCESS"

{-# COMPLETE
  OperationStatus_FAILED,
  OperationStatus_IN_PROGRESS,
  OperationStatus_PENDING,
  OperationStatus_SUCCESS,
  OperationStatus'
  #-}
