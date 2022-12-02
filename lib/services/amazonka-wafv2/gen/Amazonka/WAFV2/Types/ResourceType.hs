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
-- Module      : Amazonka.WAFV2.Types.ResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_API_GATEWAY,
        ResourceType_APPLICATION_LOAD_BALANCER,
        ResourceType_APPSYNC,
        ResourceType_COGNITO_USER_POOL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
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

pattern ResourceType_API_GATEWAY :: ResourceType
pattern ResourceType_API_GATEWAY = ResourceType' "API_GATEWAY"

pattern ResourceType_APPLICATION_LOAD_BALANCER :: ResourceType
pattern ResourceType_APPLICATION_LOAD_BALANCER = ResourceType' "APPLICATION_LOAD_BALANCER"

pattern ResourceType_APPSYNC :: ResourceType
pattern ResourceType_APPSYNC = ResourceType' "APPSYNC"

pattern ResourceType_COGNITO_USER_POOL :: ResourceType
pattern ResourceType_COGNITO_USER_POOL = ResourceType' "COGNITO_USER_POOL"

{-# COMPLETE
  ResourceType_API_GATEWAY,
  ResourceType_APPLICATION_LOAD_BALANCER,
  ResourceType_APPSYNC,
  ResourceType_COGNITO_USER_POOL,
  ResourceType'
  #-}
