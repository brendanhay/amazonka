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
-- Module      : Amazonka.OpenSearchServerless.Types.VpcEndpointStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.VpcEndpointStatus
  ( VpcEndpointStatus
      ( ..,
        VpcEndpointStatus_ACTIVE,
        VpcEndpointStatus_DELETING,
        VpcEndpointStatus_FAILED,
        VpcEndpointStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VpcEndpointStatus = VpcEndpointStatus'
  { fromVpcEndpointStatus ::
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

pattern VpcEndpointStatus_ACTIVE :: VpcEndpointStatus
pattern VpcEndpointStatus_ACTIVE = VpcEndpointStatus' "ACTIVE"

pattern VpcEndpointStatus_DELETING :: VpcEndpointStatus
pattern VpcEndpointStatus_DELETING = VpcEndpointStatus' "DELETING"

pattern VpcEndpointStatus_FAILED :: VpcEndpointStatus
pattern VpcEndpointStatus_FAILED = VpcEndpointStatus' "FAILED"

pattern VpcEndpointStatus_PENDING :: VpcEndpointStatus
pattern VpcEndpointStatus_PENDING = VpcEndpointStatus' "PENDING"

{-# COMPLETE
  VpcEndpointStatus_ACTIVE,
  VpcEndpointStatus_DELETING,
  VpcEndpointStatus_FAILED,
  VpcEndpointStatus_PENDING,
  VpcEndpointStatus'
  #-}
