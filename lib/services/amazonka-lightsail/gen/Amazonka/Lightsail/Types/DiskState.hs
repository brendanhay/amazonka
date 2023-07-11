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
-- Module      : Amazonka.Lightsail.Types.DiskState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DiskState
  ( DiskState
      ( ..,
        DiskState_Available,
        DiskState_Error,
        DiskState_In_use,
        DiskState_Pending,
        DiskState_Unknown
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DiskState = DiskState'
  { fromDiskState ::
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

pattern DiskState_Available :: DiskState
pattern DiskState_Available = DiskState' "available"

pattern DiskState_Error :: DiskState
pattern DiskState_Error = DiskState' "error"

pattern DiskState_In_use :: DiskState
pattern DiskState_In_use = DiskState' "in-use"

pattern DiskState_Pending :: DiskState
pattern DiskState_Pending = DiskState' "pending"

pattern DiskState_Unknown :: DiskState
pattern DiskState_Unknown = DiskState' "unknown"

{-# COMPLETE
  DiskState_Available,
  DiskState_Error,
  DiskState_In_use,
  DiskState_Pending,
  DiskState_Unknown,
  DiskState'
  #-}
