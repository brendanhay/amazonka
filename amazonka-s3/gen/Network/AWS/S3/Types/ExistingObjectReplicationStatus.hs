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
-- Module      : Network.AWS.S3.Types.ExistingObjectReplicationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ExistingObjectReplicationStatus
  ( ExistingObjectReplicationStatus
      ( ..,
        ExistingObjectReplicationStatus_Disabled,
        ExistingObjectReplicationStatus_Enabled
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.S3.Internal

newtype ExistingObjectReplicationStatus = ExistingObjectReplicationStatus'
  { fromExistingObjectReplicationStatus ::
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

pattern ExistingObjectReplicationStatus_Disabled :: ExistingObjectReplicationStatus
pattern ExistingObjectReplicationStatus_Disabled = ExistingObjectReplicationStatus' "Disabled"

pattern ExistingObjectReplicationStatus_Enabled :: ExistingObjectReplicationStatus
pattern ExistingObjectReplicationStatus_Enabled = ExistingObjectReplicationStatus' "Enabled"

{-# COMPLETE
  ExistingObjectReplicationStatus_Disabled,
  ExistingObjectReplicationStatus_Enabled,
  ExistingObjectReplicationStatus'
  #-}
