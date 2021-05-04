{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskState
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

import qualified Network.AWS.Prelude as Prelude

newtype DiskState = DiskState'
  { fromDiskState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
