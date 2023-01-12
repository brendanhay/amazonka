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
-- Module      : Amazonka.EC2.Types.ClientVpnEndpointAttributeStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnEndpointAttributeStatusCode
  ( ClientVpnEndpointAttributeStatusCode
      ( ..,
        ClientVpnEndpointAttributeStatusCode_Applied,
        ClientVpnEndpointAttributeStatusCode_Applying
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ClientVpnEndpointAttributeStatusCode = ClientVpnEndpointAttributeStatusCode'
  { fromClientVpnEndpointAttributeStatusCode ::
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

pattern ClientVpnEndpointAttributeStatusCode_Applied :: ClientVpnEndpointAttributeStatusCode
pattern ClientVpnEndpointAttributeStatusCode_Applied = ClientVpnEndpointAttributeStatusCode' "applied"

pattern ClientVpnEndpointAttributeStatusCode_Applying :: ClientVpnEndpointAttributeStatusCode
pattern ClientVpnEndpointAttributeStatusCode_Applying = ClientVpnEndpointAttributeStatusCode' "applying"

{-# COMPLETE
  ClientVpnEndpointAttributeStatusCode_Applied,
  ClientVpnEndpointAttributeStatusCode_Applying,
  ClientVpnEndpointAttributeStatusCode'
  #-}
