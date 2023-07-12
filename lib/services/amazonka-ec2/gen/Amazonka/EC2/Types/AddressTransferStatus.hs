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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype AddressTransferStatus = AddressTransferStatus'
  { fromAddressTransferStatus ::
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
