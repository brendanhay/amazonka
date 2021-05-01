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
-- Module      : Network.AWS.EKS.Types.UpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.UpdateStatus
  ( UpdateStatus
      ( ..,
        UpdateStatus_Cancelled,
        UpdateStatus_Failed,
        UpdateStatus_InProgress,
        UpdateStatus_Successful
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype UpdateStatus = UpdateStatus'
  { fromUpdateStatus ::
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

pattern UpdateStatus_Cancelled :: UpdateStatus
pattern UpdateStatus_Cancelled = UpdateStatus' "Cancelled"

pattern UpdateStatus_Failed :: UpdateStatus
pattern UpdateStatus_Failed = UpdateStatus' "Failed"

pattern UpdateStatus_InProgress :: UpdateStatus
pattern UpdateStatus_InProgress = UpdateStatus' "InProgress"

pattern UpdateStatus_Successful :: UpdateStatus
pattern UpdateStatus_Successful = UpdateStatus' "Successful"

{-# COMPLETE
  UpdateStatus_Cancelled,
  UpdateStatus_Failed,
  UpdateStatus_InProgress,
  UpdateStatus_Successful,
  UpdateStatus'
  #-}
