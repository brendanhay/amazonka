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
-- Module      : Amazonka.APIGateway.Types.VpcLinkStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.VpcLinkStatus
  ( VpcLinkStatus
      ( ..,
        VpcLinkStatus_AVAILABLE,
        VpcLinkStatus_DELETING,
        VpcLinkStatus_FAILED,
        VpcLinkStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype VpcLinkStatus = VpcLinkStatus'
  { fromVpcLinkStatus ::
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

pattern VpcLinkStatus_AVAILABLE :: VpcLinkStatus
pattern VpcLinkStatus_AVAILABLE = VpcLinkStatus' "AVAILABLE"

pattern VpcLinkStatus_DELETING :: VpcLinkStatus
pattern VpcLinkStatus_DELETING = VpcLinkStatus' "DELETING"

pattern VpcLinkStatus_FAILED :: VpcLinkStatus
pattern VpcLinkStatus_FAILED = VpcLinkStatus' "FAILED"

pattern VpcLinkStatus_PENDING :: VpcLinkStatus
pattern VpcLinkStatus_PENDING = VpcLinkStatus' "PENDING"

{-# COMPLETE
  VpcLinkStatus_AVAILABLE,
  VpcLinkStatus_DELETING,
  VpcLinkStatus_FAILED,
  VpcLinkStatus_PENDING,
  VpcLinkStatus'
  #-}
