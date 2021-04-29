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
-- Module      : Network.AWS.EC2.Types.ServiceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceState
  ( ServiceState
      ( ..,
        ServiceState_Available,
        ServiceState_Deleted,
        ServiceState_Deleting,
        ServiceState_Failed,
        ServiceState_Pending
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ServiceState = ServiceState'
  { fromServiceState ::
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

pattern ServiceState_Available :: ServiceState
pattern ServiceState_Available = ServiceState' "Available"

pattern ServiceState_Deleted :: ServiceState
pattern ServiceState_Deleted = ServiceState' "Deleted"

pattern ServiceState_Deleting :: ServiceState
pattern ServiceState_Deleting = ServiceState' "Deleting"

pattern ServiceState_Failed :: ServiceState
pattern ServiceState_Failed = ServiceState' "Failed"

pattern ServiceState_Pending :: ServiceState
pattern ServiceState_Pending = ServiceState' "Pending"

{-# COMPLETE
  ServiceState_Available,
  ServiceState_Deleted,
  ServiceState_Deleting,
  ServiceState_Failed,
  ServiceState_Pending,
  ServiceState'
  #-}
