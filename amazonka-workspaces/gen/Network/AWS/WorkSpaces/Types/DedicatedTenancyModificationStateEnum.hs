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
-- Module      : Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
  ( DedicatedTenancyModificationStateEnum
      ( ..,
        DedicatedTenancyModificationStateEnum_COMPLETED,
        DedicatedTenancyModificationStateEnum_FAILED,
        DedicatedTenancyModificationStateEnum_PENDING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DedicatedTenancyModificationStateEnum = DedicatedTenancyModificationStateEnum'
  { fromDedicatedTenancyModificationStateEnum ::
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

pattern DedicatedTenancyModificationStateEnum_COMPLETED :: DedicatedTenancyModificationStateEnum
pattern DedicatedTenancyModificationStateEnum_COMPLETED = DedicatedTenancyModificationStateEnum' "COMPLETED"

pattern DedicatedTenancyModificationStateEnum_FAILED :: DedicatedTenancyModificationStateEnum
pattern DedicatedTenancyModificationStateEnum_FAILED = DedicatedTenancyModificationStateEnum' "FAILED"

pattern DedicatedTenancyModificationStateEnum_PENDING :: DedicatedTenancyModificationStateEnum
pattern DedicatedTenancyModificationStateEnum_PENDING = DedicatedTenancyModificationStateEnum' "PENDING"

{-# COMPLETE
  DedicatedTenancyModificationStateEnum_COMPLETED,
  DedicatedTenancyModificationStateEnum_FAILED,
  DedicatedTenancyModificationStateEnum_PENDING,
  DedicatedTenancyModificationStateEnum'
  #-}
