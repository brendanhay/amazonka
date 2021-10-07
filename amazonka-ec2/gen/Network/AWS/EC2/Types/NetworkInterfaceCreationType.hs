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
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceCreationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceCreationType
  ( NetworkInterfaceCreationType
      ( ..,
        NetworkInterfaceCreationType_Branch,
        NetworkInterfaceCreationType_Efa,
        NetworkInterfaceCreationType_Trunk
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype NetworkInterfaceCreationType = NetworkInterfaceCreationType'
  { fromNetworkInterfaceCreationType ::
      Core.Text
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

pattern NetworkInterfaceCreationType_Branch :: NetworkInterfaceCreationType
pattern NetworkInterfaceCreationType_Branch = NetworkInterfaceCreationType' "branch"

pattern NetworkInterfaceCreationType_Efa :: NetworkInterfaceCreationType
pattern NetworkInterfaceCreationType_Efa = NetworkInterfaceCreationType' "efa"

pattern NetworkInterfaceCreationType_Trunk :: NetworkInterfaceCreationType
pattern NetworkInterfaceCreationType_Trunk = NetworkInterfaceCreationType' "trunk"

{-# COMPLETE
  NetworkInterfaceCreationType_Branch,
  NetworkInterfaceCreationType_Efa,
  NetworkInterfaceCreationType_Trunk,
  NetworkInterfaceCreationType'
  #-}
