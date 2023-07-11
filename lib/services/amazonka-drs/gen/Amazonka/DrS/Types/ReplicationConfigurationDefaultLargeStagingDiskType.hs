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
-- Module      : Amazonka.DrS.Types.ReplicationConfigurationDefaultLargeStagingDiskType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ReplicationConfigurationDefaultLargeStagingDiskType
  ( ReplicationConfigurationDefaultLargeStagingDiskType
      ( ..,
        ReplicationConfigurationDefaultLargeStagingDiskType_AUTO,
        ReplicationConfigurationDefaultLargeStagingDiskType_GP2,
        ReplicationConfigurationDefaultLargeStagingDiskType_GP3,
        ReplicationConfigurationDefaultLargeStagingDiskType_ST1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReplicationConfigurationDefaultLargeStagingDiskType = ReplicationConfigurationDefaultLargeStagingDiskType'
  { fromReplicationConfigurationDefaultLargeStagingDiskType ::
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

pattern ReplicationConfigurationDefaultLargeStagingDiskType_AUTO :: ReplicationConfigurationDefaultLargeStagingDiskType
pattern ReplicationConfigurationDefaultLargeStagingDiskType_AUTO = ReplicationConfigurationDefaultLargeStagingDiskType' "AUTO"

pattern ReplicationConfigurationDefaultLargeStagingDiskType_GP2 :: ReplicationConfigurationDefaultLargeStagingDiskType
pattern ReplicationConfigurationDefaultLargeStagingDiskType_GP2 = ReplicationConfigurationDefaultLargeStagingDiskType' "GP2"

pattern ReplicationConfigurationDefaultLargeStagingDiskType_GP3 :: ReplicationConfigurationDefaultLargeStagingDiskType
pattern ReplicationConfigurationDefaultLargeStagingDiskType_GP3 = ReplicationConfigurationDefaultLargeStagingDiskType' "GP3"

pattern ReplicationConfigurationDefaultLargeStagingDiskType_ST1 :: ReplicationConfigurationDefaultLargeStagingDiskType
pattern ReplicationConfigurationDefaultLargeStagingDiskType_ST1 = ReplicationConfigurationDefaultLargeStagingDiskType' "ST1"

{-# COMPLETE
  ReplicationConfigurationDefaultLargeStagingDiskType_AUTO,
  ReplicationConfigurationDefaultLargeStagingDiskType_GP2,
  ReplicationConfigurationDefaultLargeStagingDiskType_GP3,
  ReplicationConfigurationDefaultLargeStagingDiskType_ST1,
  ReplicationConfigurationDefaultLargeStagingDiskType'
  #-}
