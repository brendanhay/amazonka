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
-- Module      : Network.AWS.Transfer.Types.EndpointType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transfer.Types.EndpointType
  ( EndpointType
      ( ..,
        EndpointType_PUBLIC,
        EndpointType_VPC,
        EndpointType_VPC_ENDPOINT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EndpointType = EndpointType'
  { fromEndpointType ::
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
