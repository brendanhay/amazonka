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
-- Module      : Amazonka.EC2.Types.NetworkInterfaceStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfaceStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype NetworkInterfaceStatus = NetworkInterfaceStatus'
  { fromNetworkInterfaceStatus ::
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
