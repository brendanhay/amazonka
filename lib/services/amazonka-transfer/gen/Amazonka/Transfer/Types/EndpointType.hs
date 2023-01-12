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
-- Module      : Amazonka.Transfer.Types.EndpointType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.EndpointType
  ( EndpointType
      ( ..,
        EndpointType_PUBLIC,
        EndpointType_VPC,
        EndpointType_VPC_ENDPOINT
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

pattern EndpointType_PUBLIC :: EndpointType
pattern EndpointType_PUBLIC = EndpointType' "PUBLIC"

pattern EndpointType_VPC :: EndpointType
pattern EndpointType_VPC = EndpointType' "VPC"

pattern EndpointType_VPC_ENDPOINT :: EndpointType
pattern EndpointType_VPC_ENDPOINT = EndpointType' "VPC_ENDPOINT"

{-# COMPLETE
  EndpointType_PUBLIC,
  EndpointType_VPC,
  EndpointType_VPC_ENDPOINT,
  EndpointType'
  #-}
