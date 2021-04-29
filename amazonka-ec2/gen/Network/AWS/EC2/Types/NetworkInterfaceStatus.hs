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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype NetworkInterfaceStatus = NetworkInterfaceStatus'
  { fromNetworkInterfaceStatus ::
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
