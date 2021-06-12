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
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationStatus
  ( OperationStatus
      ( ..,
        OperationStatus_FAIL,
        OperationStatus_PENDING,
        OperationStatus_SUBMITTED,
        OperationStatus_SUCCESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OperationStatus = OperationStatus'
  { fromOperationStatus ::
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

pattern OperationStatus_FAIL :: OperationStatus
pattern OperationStatus_FAIL = OperationStatus' "FAIL"

pattern OperationStatus_PENDING :: OperationStatus
pattern OperationStatus_PENDING = OperationStatus' "PENDING"

pattern OperationStatus_SUBMITTED :: OperationStatus
pattern OperationStatus_SUBMITTED = OperationStatus' "SUBMITTED"

pattern OperationStatus_SUCCESS :: OperationStatus
pattern OperationStatus_SUCCESS = OperationStatus' "SUCCESS"

{-# COMPLETE
  OperationStatus_FAIL,
  OperationStatus_PENDING,
  OperationStatus_SUBMITTED,
  OperationStatus_SUCCESS,
  OperationStatus'
  #-}
