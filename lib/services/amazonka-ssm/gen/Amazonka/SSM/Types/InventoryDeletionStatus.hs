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
-- Module      : Amazonka.SSM.Types.InventoryDeletionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryDeletionStatus
  ( InventoryDeletionStatus
      ( ..,
        InventoryDeletionStatus_Complete,
        InventoryDeletionStatus_InProgress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InventoryDeletionStatus = InventoryDeletionStatus'
  { fromInventoryDeletionStatus ::
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

pattern InventoryDeletionStatus_Complete :: InventoryDeletionStatus
pattern InventoryDeletionStatus_Complete = InventoryDeletionStatus' "Complete"

pattern InventoryDeletionStatus_InProgress :: InventoryDeletionStatus
pattern InventoryDeletionStatus_InProgress = InventoryDeletionStatus' "InProgress"

{-# COMPLETE
  InventoryDeletionStatus_Complete,
  InventoryDeletionStatus_InProgress,
  InventoryDeletionStatus'
  #-}
