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
-- Module      : Network.AWS.SSM.Types.LastResourceDataSyncStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.LastResourceDataSyncStatus
  ( LastResourceDataSyncStatus
      ( ..,
        LastResourceDataSyncStatus_Failed,
        LastResourceDataSyncStatus_InProgress,
        LastResourceDataSyncStatus_Successful
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype LastResourceDataSyncStatus = LastResourceDataSyncStatus'
  { fromLastResourceDataSyncStatus ::
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

pattern LastResourceDataSyncStatus_Failed :: LastResourceDataSyncStatus
pattern LastResourceDataSyncStatus_Failed = LastResourceDataSyncStatus' "Failed"

pattern LastResourceDataSyncStatus_InProgress :: LastResourceDataSyncStatus
pattern LastResourceDataSyncStatus_InProgress = LastResourceDataSyncStatus' "InProgress"

pattern LastResourceDataSyncStatus_Successful :: LastResourceDataSyncStatus
pattern LastResourceDataSyncStatus_Successful = LastResourceDataSyncStatus' "Successful"

{-# COMPLETE
  LastResourceDataSyncStatus_Failed,
  LastResourceDataSyncStatus_InProgress,
  LastResourceDataSyncStatus_Successful,
  LastResourceDataSyncStatus'
  #-}
