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
-- Module      : Amazonka.ApiGatewayV2.Types.VpcLinkStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.VpcLinkStatus
  ( VpcLinkStatus
      ( ..,
        VpcLinkStatus_AVAILABLE,
        VpcLinkStatus_DELETING,
        VpcLinkStatus_FAILED,
        VpcLinkStatus_INACTIVE,
        VpcLinkStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of the VPC link.
newtype VpcLinkStatus = VpcLinkStatus'
  { fromVpcLinkStatus ::
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

pattern VpcLinkStatus_AVAILABLE :: VpcLinkStatus
pattern VpcLinkStatus_AVAILABLE = VpcLinkStatus' "AVAILABLE"

pattern VpcLinkStatus_DELETING :: VpcLinkStatus
pattern VpcLinkStatus_DELETING = VpcLinkStatus' "DELETING"

pattern VpcLinkStatus_FAILED :: VpcLinkStatus
pattern VpcLinkStatus_FAILED = VpcLinkStatus' "FAILED"

pattern VpcLinkStatus_INACTIVE :: VpcLinkStatus
pattern VpcLinkStatus_INACTIVE = VpcLinkStatus' "INACTIVE"

pattern VpcLinkStatus_PENDING :: VpcLinkStatus
pattern VpcLinkStatus_PENDING = VpcLinkStatus' "PENDING"

{-# COMPLETE
  VpcLinkStatus_AVAILABLE,
  VpcLinkStatus_DELETING,
  VpcLinkStatus_FAILED,
  VpcLinkStatus_INACTIVE,
  VpcLinkStatus_PENDING,
  VpcLinkStatus'
  #-}
