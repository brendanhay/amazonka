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
-- Module      : Amazonka.ElasticBeanstalk.Types.PlatformStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.PlatformStatus
  ( PlatformStatus
      ( ..,
        PlatformStatus_Creating,
        PlatformStatus_Deleted,
        PlatformStatus_Deleting,
        PlatformStatus_Failed,
        PlatformStatus_Ready
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PlatformStatus = PlatformStatus'
  { fromPlatformStatus ::
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

pattern PlatformStatus_Creating :: PlatformStatus
pattern PlatformStatus_Creating = PlatformStatus' "Creating"

pattern PlatformStatus_Deleted :: PlatformStatus
pattern PlatformStatus_Deleted = PlatformStatus' "Deleted"

pattern PlatformStatus_Deleting :: PlatformStatus
pattern PlatformStatus_Deleting = PlatformStatus' "Deleting"

pattern PlatformStatus_Failed :: PlatformStatus
pattern PlatformStatus_Failed = PlatformStatus' "Failed"

pattern PlatformStatus_Ready :: PlatformStatus
pattern PlatformStatus_Ready = PlatformStatus' "Ready"

{-# COMPLETE
  PlatformStatus_Creating,
  PlatformStatus_Deleted,
  PlatformStatus_Deleting,
  PlatformStatus_Failed,
  PlatformStatus_Ready,
  PlatformStatus'
  #-}
