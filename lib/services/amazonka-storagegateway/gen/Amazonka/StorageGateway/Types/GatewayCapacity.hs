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
-- Module      : Amazonka.StorageGateway.Types.GatewayCapacity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.GatewayCapacity
  ( GatewayCapacity
      ( ..,
        GatewayCapacity_Large,
        GatewayCapacity_Medium,
        GatewayCapacity_Small
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GatewayCapacity = GatewayCapacity'
  { fromGatewayCapacity ::
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

pattern GatewayCapacity_Large :: GatewayCapacity
pattern GatewayCapacity_Large = GatewayCapacity' "Large"

pattern GatewayCapacity_Medium :: GatewayCapacity
pattern GatewayCapacity_Medium = GatewayCapacity' "Medium"

pattern GatewayCapacity_Small :: GatewayCapacity
pattern GatewayCapacity_Small = GatewayCapacity' "Small"

{-# COMPLETE
  GatewayCapacity_Large,
  GatewayCapacity_Medium,
  GatewayCapacity_Small,
  GatewayCapacity'
  #-}
