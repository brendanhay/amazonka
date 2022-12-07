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
-- Module      : Amazonka.ApiGatewayV2.Types.SecurityPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.SecurityPolicy
  ( SecurityPolicy
      ( ..,
        SecurityPolicy_TLS_1_0,
        SecurityPolicy_TLS_1_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Transport Layer Security (TLS) version of the security policy for
-- this domain name. The valid values are TLS_1_0 and TLS_1_2.
newtype SecurityPolicy = SecurityPolicy'
  { fromSecurityPolicy ::
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

pattern SecurityPolicy_TLS_1_0 :: SecurityPolicy
pattern SecurityPolicy_TLS_1_0 = SecurityPolicy' "TLS_1_0"

pattern SecurityPolicy_TLS_1_2 :: SecurityPolicy
pattern SecurityPolicy_TLS_1_2 = SecurityPolicy' "TLS_1_2"

{-# COMPLETE
  SecurityPolicy_TLS_1_0,
  SecurityPolicy_TLS_1_2,
  SecurityPolicy'
  #-}
