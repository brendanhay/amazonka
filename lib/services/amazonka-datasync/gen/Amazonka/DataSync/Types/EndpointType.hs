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
-- Module      : Amazonka.DataSync.Types.EndpointType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.EndpointType
  ( EndpointType
      ( ..,
        EndpointType_FIPS,
        EndpointType_PRIVATE_LINK,
        EndpointType_PUBLIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EndpointType = EndpointType'
  { fromEndpointType ::
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

pattern EndpointType_FIPS :: EndpointType
pattern EndpointType_FIPS = EndpointType' "FIPS"

pattern EndpointType_PRIVATE_LINK :: EndpointType
pattern EndpointType_PRIVATE_LINK = EndpointType' "PRIVATE_LINK"

pattern EndpointType_PUBLIC :: EndpointType
pattern EndpointType_PUBLIC = EndpointType' "PUBLIC"

{-# COMPLETE
  EndpointType_FIPS,
  EndpointType_PRIVATE_LINK,
  EndpointType_PUBLIC,
  EndpointType'
  #-}
