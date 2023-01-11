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
-- Module      : Amazonka.EC2.Types.ClientVpnRouteStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnRouteStatusCode
  ( ClientVpnRouteStatusCode
      ( ..,
        ClientVpnRouteStatusCode_Active,
        ClientVpnRouteStatusCode_Creating,
        ClientVpnRouteStatusCode_Deleting,
        ClientVpnRouteStatusCode_Failed
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ClientVpnRouteStatusCode = ClientVpnRouteStatusCode'
  { fromClientVpnRouteStatusCode ::
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

pattern ClientVpnRouteStatusCode_Active :: ClientVpnRouteStatusCode
pattern ClientVpnRouteStatusCode_Active = ClientVpnRouteStatusCode' "active"

pattern ClientVpnRouteStatusCode_Creating :: ClientVpnRouteStatusCode
pattern ClientVpnRouteStatusCode_Creating = ClientVpnRouteStatusCode' "creating"

pattern ClientVpnRouteStatusCode_Deleting :: ClientVpnRouteStatusCode
pattern ClientVpnRouteStatusCode_Deleting = ClientVpnRouteStatusCode' "deleting"

pattern ClientVpnRouteStatusCode_Failed :: ClientVpnRouteStatusCode
pattern ClientVpnRouteStatusCode_Failed = ClientVpnRouteStatusCode' "failed"

{-# COMPLETE
  ClientVpnRouteStatusCode_Active,
  ClientVpnRouteStatusCode_Creating,
  ClientVpnRouteStatusCode_Deleting,
  ClientVpnRouteStatusCode_Failed,
  ClientVpnRouteStatusCode'
  #-}
