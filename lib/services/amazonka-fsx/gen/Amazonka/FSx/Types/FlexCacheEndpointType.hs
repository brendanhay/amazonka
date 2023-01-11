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
-- Module      : Amazonka.FSx.Types.FlexCacheEndpointType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FlexCacheEndpointType
  ( FlexCacheEndpointType
      ( ..,
        FlexCacheEndpointType_CACHE,
        FlexCacheEndpointType_NONE,
        FlexCacheEndpointType_ORIGIN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FlexCacheEndpointType = FlexCacheEndpointType'
  { fromFlexCacheEndpointType ::
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

pattern FlexCacheEndpointType_CACHE :: FlexCacheEndpointType
pattern FlexCacheEndpointType_CACHE = FlexCacheEndpointType' "CACHE"

pattern FlexCacheEndpointType_NONE :: FlexCacheEndpointType
pattern FlexCacheEndpointType_NONE = FlexCacheEndpointType' "NONE"

pattern FlexCacheEndpointType_ORIGIN :: FlexCacheEndpointType
pattern FlexCacheEndpointType_ORIGIN = FlexCacheEndpointType' "ORIGIN"

{-# COMPLETE
  FlexCacheEndpointType_CACHE,
  FlexCacheEndpointType_NONE,
  FlexCacheEndpointType_ORIGIN,
  FlexCacheEndpointType'
  #-}
