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
-- Module      : Amazonka.EC2.Types.AddressTransferStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AddressTransferStatus
  ( AddressTransferStatus
      ( ..,
        AddressTransferStatus_Accepted,
        AddressTransferStatus_Disabled,
        AddressTransferStatus_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype AddressTransferStatus = AddressTransferStatus'
  { fromAddressTransferStatus ::
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

pattern AddressTransferStatus_Accepted :: AddressTransferStatus
pattern AddressTransferStatus_Accepted = AddressTransferStatus' "accepted"

pattern AddressTransferStatus_Disabled :: AddressTransferStatus
pattern AddressTransferStatus_Disabled = AddressTransferStatus' "disabled"

pattern AddressTransferStatus_Pending :: AddressTransferStatus
pattern AddressTransferStatus_Pending = AddressTransferStatus' "pending"

{-# COMPLETE
  AddressTransferStatus_Accepted,
  AddressTransferStatus_Disabled,
  AddressTransferStatus_Pending,
  AddressTransferStatus'
  #-}
