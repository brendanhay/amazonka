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
-- Module      : Network.AWS.DeviceFarm.Types.InstanceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.InstanceStatus
  ( InstanceStatus
      ( ..,
        InstanceStatus_AVAILABLE,
        InstanceStatus_IN_USE,
        InstanceStatus_NOT_AVAILABLE,
        InstanceStatus_PREPARING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype InstanceStatus = InstanceStatus'
  { fromInstanceStatus ::
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

pattern InstanceStatus_AVAILABLE :: InstanceStatus
pattern InstanceStatus_AVAILABLE = InstanceStatus' "AVAILABLE"

pattern InstanceStatus_IN_USE :: InstanceStatus
pattern InstanceStatus_IN_USE = InstanceStatus' "IN_USE"

pattern InstanceStatus_NOT_AVAILABLE :: InstanceStatus
pattern InstanceStatus_NOT_AVAILABLE = InstanceStatus' "NOT_AVAILABLE"

pattern InstanceStatus_PREPARING :: InstanceStatus
pattern InstanceStatus_PREPARING = InstanceStatus' "PREPARING"

{-# COMPLETE
  InstanceStatus_AVAILABLE,
  InstanceStatus_IN_USE,
  InstanceStatus_NOT_AVAILABLE,
  InstanceStatus_PREPARING,
  InstanceStatus'
  #-}
