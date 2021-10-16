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
-- Module      : Network.AWS.S3.Types.ReplicaModificationsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicaModificationsStatus
  ( ReplicaModificationsStatus
      ( ..,
        ReplicaModificationsStatus_Disabled,
        ReplicaModificationsStatus_Enabled
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

newtype ReplicaModificationsStatus = ReplicaModificationsStatus'
  { fromReplicaModificationsStatus ::
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

pattern ReplicaModificationsStatus_Disabled :: ReplicaModificationsStatus
pattern ReplicaModificationsStatus_Disabled = ReplicaModificationsStatus' "Disabled"

pattern ReplicaModificationsStatus_Enabled :: ReplicaModificationsStatus
pattern ReplicaModificationsStatus_Enabled = ReplicaModificationsStatus' "Enabled"

{-# COMPLETE
  ReplicaModificationsStatus_Disabled,
  ReplicaModificationsStatus_Enabled,
  ReplicaModificationsStatus'
  #-}
