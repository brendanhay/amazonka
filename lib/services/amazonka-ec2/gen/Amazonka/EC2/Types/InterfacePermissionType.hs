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
-- Module      : Amazonka.EC2.Types.InterfacePermissionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InterfacePermissionType
  ( InterfacePermissionType
      ( ..,
        InterfacePermissionType_EIP_ASSOCIATE,
        InterfacePermissionType_INSTANCE_ATTACH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype InterfacePermissionType = InterfacePermissionType'
  { fromInterfacePermissionType ::
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

pattern InterfacePermissionType_EIP_ASSOCIATE :: InterfacePermissionType
pattern InterfacePermissionType_EIP_ASSOCIATE = InterfacePermissionType' "EIP-ASSOCIATE"

pattern InterfacePermissionType_INSTANCE_ATTACH :: InterfacePermissionType
pattern InterfacePermissionType_INSTANCE_ATTACH = InterfacePermissionType' "INSTANCE-ATTACH"

{-# COMPLETE
  InterfacePermissionType_EIP_ASSOCIATE,
  InterfacePermissionType_INSTANCE_ATTACH,
  InterfacePermissionType'
  #-}
