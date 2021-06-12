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
-- Module      : Network.AWS.EC2.Types.AddressStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AddressStatus
  ( AddressStatus
      ( ..,
        AddressStatus_InClassic,
        AddressStatus_InVpc,
        AddressStatus_MoveInProgress
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype AddressStatus = AddressStatus'
  { fromAddressStatus ::
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

pattern AddressStatus_InClassic :: AddressStatus
pattern AddressStatus_InClassic = AddressStatus' "InClassic"

pattern AddressStatus_InVpc :: AddressStatus
pattern AddressStatus_InVpc = AddressStatus' "InVpc"

pattern AddressStatus_MoveInProgress :: AddressStatus
pattern AddressStatus_MoveInProgress = AddressStatus' "MoveInProgress"

{-# COMPLETE
  AddressStatus_InClassic,
  AddressStatus_InVpc,
  AddressStatus_MoveInProgress,
  AddressStatus'
  #-}
