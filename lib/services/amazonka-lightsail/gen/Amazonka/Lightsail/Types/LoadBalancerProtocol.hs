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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerProtocol
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerProtocol
  ( LoadBalancerProtocol
      ( ..,
        LoadBalancerProtocol_HTTP,
        LoadBalancerProtocol_HTTP_HTTPS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LoadBalancerProtocol = LoadBalancerProtocol'
  { fromLoadBalancerProtocol ::
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

pattern LoadBalancerProtocol_HTTP :: LoadBalancerProtocol
pattern LoadBalancerProtocol_HTTP = LoadBalancerProtocol' "HTTP"

pattern LoadBalancerProtocol_HTTP_HTTPS :: LoadBalancerProtocol
pattern LoadBalancerProtocol_HTTP_HTTPS = LoadBalancerProtocol' "HTTP_HTTPS"

{-# COMPLETE
  LoadBalancerProtocol_HTTP,
  LoadBalancerProtocol_HTTP_HTTPS,
  LoadBalancerProtocol'
  #-}
