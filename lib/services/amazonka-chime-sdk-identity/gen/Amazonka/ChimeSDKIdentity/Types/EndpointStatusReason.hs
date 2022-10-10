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
-- Module      : Amazonka.ChimeSDKIdentity.Types.EndpointStatusReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.EndpointStatusReason
  ( EndpointStatusReason
      ( ..,
        EndpointStatusReason_INVALID_DEVICE_TOKEN,
        EndpointStatusReason_INVALID_PINPOINT_ARN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EndpointStatusReason = EndpointStatusReason'
  { fromEndpointStatusReason ::
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

pattern EndpointStatusReason_INVALID_DEVICE_TOKEN :: EndpointStatusReason
pattern EndpointStatusReason_INVALID_DEVICE_TOKEN = EndpointStatusReason' "INVALID_DEVICE_TOKEN"

pattern EndpointStatusReason_INVALID_PINPOINT_ARN :: EndpointStatusReason
pattern EndpointStatusReason_INVALID_PINPOINT_ARN = EndpointStatusReason' "INVALID_PINPOINT_ARN"

{-# COMPLETE
  EndpointStatusReason_INVALID_DEVICE_TOKEN,
  EndpointStatusReason_INVALID_PINPOINT_ARN,
  EndpointStatusReason'
  #-}
