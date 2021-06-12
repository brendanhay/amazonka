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
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceStatus
  ( NetworkInterfaceStatus
      ( ..,
        NetworkInterfaceStatus_Associated,
        NetworkInterfaceStatus_Attaching,
        NetworkInterfaceStatus_Available,
        NetworkInterfaceStatus_Detaching,
        NetworkInterfaceStatus_In_use
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype NetworkInterfaceStatus = NetworkInterfaceStatus'
  { fromNetworkInterfaceStatus ::
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

pattern NetworkInterfaceStatus_Associated :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_Associated = NetworkInterfaceStatus' "associated"

pattern NetworkInterfaceStatus_Attaching :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_Attaching = NetworkInterfaceStatus' "attaching"

pattern NetworkInterfaceStatus_Available :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_Available = NetworkInterfaceStatus' "available"

pattern NetworkInterfaceStatus_Detaching :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_Detaching = NetworkInterfaceStatus' "detaching"

pattern NetworkInterfaceStatus_In_use :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_In_use = NetworkInterfaceStatus' "in-use"

{-# COMPLETE
  NetworkInterfaceStatus_Associated,
  NetworkInterfaceStatus_Attaching,
  NetworkInterfaceStatus_Available,
  NetworkInterfaceStatus_Detaching,
  NetworkInterfaceStatus_In_use,
  NetworkInterfaceStatus'
  #-}
