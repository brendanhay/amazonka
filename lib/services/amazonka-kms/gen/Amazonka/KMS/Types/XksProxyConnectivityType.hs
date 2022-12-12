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
-- Module      : Amazonka.KMS.Types.XksProxyConnectivityType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.XksProxyConnectivityType
  ( XksProxyConnectivityType
      ( ..,
        XksProxyConnectivityType_PUBLIC_ENDPOINT,
        XksProxyConnectivityType_VPC_ENDPOINT_SERVICE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype XksProxyConnectivityType = XksProxyConnectivityType'
  { fromXksProxyConnectivityType ::
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

pattern XksProxyConnectivityType_PUBLIC_ENDPOINT :: XksProxyConnectivityType
pattern XksProxyConnectivityType_PUBLIC_ENDPOINT = XksProxyConnectivityType' "PUBLIC_ENDPOINT"

pattern XksProxyConnectivityType_VPC_ENDPOINT_SERVICE :: XksProxyConnectivityType
pattern XksProxyConnectivityType_VPC_ENDPOINT_SERVICE = XksProxyConnectivityType' "VPC_ENDPOINT_SERVICE"

{-# COMPLETE
  XksProxyConnectivityType_PUBLIC_ENDPOINT,
  XksProxyConnectivityType_VPC_ENDPOINT_SERVICE,
  XksProxyConnectivityType'
  #-}
