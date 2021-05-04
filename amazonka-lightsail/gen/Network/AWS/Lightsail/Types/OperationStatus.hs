{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.OperationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.OperationStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype OperationStatus = OperationStatus'
  { fromOperationStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
