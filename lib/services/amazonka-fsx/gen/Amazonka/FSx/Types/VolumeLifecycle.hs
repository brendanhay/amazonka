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
-- Module      : Amazonka.FSx.Types.VolumeLifecycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.VolumeLifecycle
  ( VolumeLifecycle
      ( ..,
        VolumeLifecycle_AVAILABLE,
        VolumeLifecycle_CREATED,
        VolumeLifecycle_CREATING,
        VolumeLifecycle_DELETING,
        VolumeLifecycle_FAILED,
        VolumeLifecycle_MISCONFIGURED,
        VolumeLifecycle_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VolumeLifecycle = VolumeLifecycle'
  { fromVolumeLifecycle ::
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

pattern VolumeLifecycle_AVAILABLE :: VolumeLifecycle
pattern VolumeLifecycle_AVAILABLE = VolumeLifecycle' "AVAILABLE"

pattern VolumeLifecycle_CREATED :: VolumeLifecycle
pattern VolumeLifecycle_CREATED = VolumeLifecycle' "CREATED"

pattern VolumeLifecycle_CREATING :: VolumeLifecycle
pattern VolumeLifecycle_CREATING = VolumeLifecycle' "CREATING"

pattern VolumeLifecycle_DELETING :: VolumeLifecycle
pattern VolumeLifecycle_DELETING = VolumeLifecycle' "DELETING"

pattern VolumeLifecycle_FAILED :: VolumeLifecycle
pattern VolumeLifecycle_FAILED = VolumeLifecycle' "FAILED"

pattern VolumeLifecycle_MISCONFIGURED :: VolumeLifecycle
pattern VolumeLifecycle_MISCONFIGURED = VolumeLifecycle' "MISCONFIGURED"

pattern VolumeLifecycle_PENDING :: VolumeLifecycle
pattern VolumeLifecycle_PENDING = VolumeLifecycle' "PENDING"

{-# COMPLETE
  VolumeLifecycle_AVAILABLE,
  VolumeLifecycle_CREATED,
  VolumeLifecycle_CREATING,
  VolumeLifecycle_DELETING,
  VolumeLifecycle_FAILED,
  VolumeLifecycle_MISCONFIGURED,
  VolumeLifecycle_PENDING,
  VolumeLifecycle'
  #-}
