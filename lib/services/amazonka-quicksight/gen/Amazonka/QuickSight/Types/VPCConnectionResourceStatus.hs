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
-- Module      : Amazonka.QuickSight.Types.VPCConnectionResourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VPCConnectionResourceStatus
  ( VPCConnectionResourceStatus
      ( ..,
        VPCConnectionResourceStatus_CREATION_FAILED,
        VPCConnectionResourceStatus_CREATION_IN_PROGRESS,
        VPCConnectionResourceStatus_CREATION_SUCCESSFUL,
        VPCConnectionResourceStatus_DELETED,
        VPCConnectionResourceStatus_DELETION_FAILED,
        VPCConnectionResourceStatus_DELETION_IN_PROGRESS,
        VPCConnectionResourceStatus_UPDATE_FAILED,
        VPCConnectionResourceStatus_UPDATE_IN_PROGRESS,
        VPCConnectionResourceStatus_UPDATE_SUCCESSFUL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VPCConnectionResourceStatus = VPCConnectionResourceStatus'
  { fromVPCConnectionResourceStatus ::
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

pattern VPCConnectionResourceStatus_CREATION_FAILED :: VPCConnectionResourceStatus
pattern VPCConnectionResourceStatus_CREATION_FAILED = VPCConnectionResourceStatus' "CREATION_FAILED"

pattern VPCConnectionResourceStatus_CREATION_IN_PROGRESS :: VPCConnectionResourceStatus
pattern VPCConnectionResourceStatus_CREATION_IN_PROGRESS = VPCConnectionResourceStatus' "CREATION_IN_PROGRESS"

pattern VPCConnectionResourceStatus_CREATION_SUCCESSFUL :: VPCConnectionResourceStatus
pattern VPCConnectionResourceStatus_CREATION_SUCCESSFUL = VPCConnectionResourceStatus' "CREATION_SUCCESSFUL"

pattern VPCConnectionResourceStatus_DELETED :: VPCConnectionResourceStatus
pattern VPCConnectionResourceStatus_DELETED = VPCConnectionResourceStatus' "DELETED"

pattern VPCConnectionResourceStatus_DELETION_FAILED :: VPCConnectionResourceStatus
pattern VPCConnectionResourceStatus_DELETION_FAILED = VPCConnectionResourceStatus' "DELETION_FAILED"

pattern VPCConnectionResourceStatus_DELETION_IN_PROGRESS :: VPCConnectionResourceStatus
pattern VPCConnectionResourceStatus_DELETION_IN_PROGRESS = VPCConnectionResourceStatus' "DELETION_IN_PROGRESS"

pattern VPCConnectionResourceStatus_UPDATE_FAILED :: VPCConnectionResourceStatus
pattern VPCConnectionResourceStatus_UPDATE_FAILED = VPCConnectionResourceStatus' "UPDATE_FAILED"

pattern VPCConnectionResourceStatus_UPDATE_IN_PROGRESS :: VPCConnectionResourceStatus
pattern VPCConnectionResourceStatus_UPDATE_IN_PROGRESS = VPCConnectionResourceStatus' "UPDATE_IN_PROGRESS"

pattern VPCConnectionResourceStatus_UPDATE_SUCCESSFUL :: VPCConnectionResourceStatus
pattern VPCConnectionResourceStatus_UPDATE_SUCCESSFUL = VPCConnectionResourceStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  VPCConnectionResourceStatus_CREATION_FAILED,
  VPCConnectionResourceStatus_CREATION_IN_PROGRESS,
  VPCConnectionResourceStatus_CREATION_SUCCESSFUL,
  VPCConnectionResourceStatus_DELETED,
  VPCConnectionResourceStatus_DELETION_FAILED,
  VPCConnectionResourceStatus_DELETION_IN_PROGRESS,
  VPCConnectionResourceStatus_UPDATE_FAILED,
  VPCConnectionResourceStatus_UPDATE_IN_PROGRESS,
  VPCConnectionResourceStatus_UPDATE_SUCCESSFUL,
  VPCConnectionResourceStatus'
  #-}
