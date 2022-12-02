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
-- Module      : Amazonka.SSM.Types.LastResourceDataSyncStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.LastResourceDataSyncStatus
  ( LastResourceDataSyncStatus
      ( ..,
        LastResourceDataSyncStatus_Failed,
        LastResourceDataSyncStatus_InProgress,
        LastResourceDataSyncStatus_Successful
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LastResourceDataSyncStatus = LastResourceDataSyncStatus'
  { fromLastResourceDataSyncStatus ::
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
