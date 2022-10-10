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
-- Module      : Amazonka.MGN.Types.ReplicationConfigurationDefaultLargeStagingDiskType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ReplicationConfigurationDefaultLargeStagingDiskType
  ( ReplicationConfigurationDefaultLargeStagingDiskType
      ( ..,
        ReplicationConfigurationDefaultLargeStagingDiskType_GP2,
        ReplicationConfigurationDefaultLargeStagingDiskType_GP3,
        ReplicationConfigurationDefaultLargeStagingDiskType_ST1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ReplicationConfigurationDefaultLargeStagingDiskType = ReplicationConfigurationDefaultLargeStagingDiskType'
  { fromReplicationConfigurationDefaultLargeStagingDiskType ::
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

pattern ReplicationConfigurationDefaultLargeStagingDiskType_GP2 :: ReplicationConfigurationDefaultLargeStagingDiskType
pattern ReplicationConfigurationDefaultLargeStagingDiskType_GP2 = ReplicationConfigurationDefaultLargeStagingDiskType' "GP2"

pattern ReplicationConfigurationDefaultLargeStagingDiskType_GP3 :: ReplicationConfigurationDefaultLargeStagingDiskType
pattern ReplicationConfigurationDefaultLargeStagingDiskType_GP3 = ReplicationConfigurationDefaultLargeStagingDiskType' "GP3"

pattern ReplicationConfigurationDefaultLargeStagingDiskType_ST1 :: ReplicationConfigurationDefaultLargeStagingDiskType
pattern ReplicationConfigurationDefaultLargeStagingDiskType_ST1 = ReplicationConfigurationDefaultLargeStagingDiskType' "ST1"

{-# COMPLETE
  ReplicationConfigurationDefaultLargeStagingDiskType_GP2,
  ReplicationConfigurationDefaultLargeStagingDiskType_GP3,
  ReplicationConfigurationDefaultLargeStagingDiskType_ST1,
  ReplicationConfigurationDefaultLargeStagingDiskType'
  #-}
