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
-- Module      : Amazonka.RAM.Types.ResourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceStatus
  ( ResourceStatus
      ( ..,
        ResourceStatus_AVAILABLE,
        ResourceStatus_LIMIT_EXCEEDED,
        ResourceStatus_PENDING,
        ResourceStatus_UNAVAILABLE,
        ResourceStatus_ZONAL_RESOURCE_INACCESSIBLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceStatus = ResourceStatus'
  { fromResourceStatus ::
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

pattern ResourceStatus_AVAILABLE :: ResourceStatus
pattern ResourceStatus_AVAILABLE = ResourceStatus' "AVAILABLE"

pattern ResourceStatus_LIMIT_EXCEEDED :: ResourceStatus
pattern ResourceStatus_LIMIT_EXCEEDED = ResourceStatus' "LIMIT_EXCEEDED"

pattern ResourceStatus_PENDING :: ResourceStatus
pattern ResourceStatus_PENDING = ResourceStatus' "PENDING"

pattern ResourceStatus_UNAVAILABLE :: ResourceStatus
pattern ResourceStatus_UNAVAILABLE = ResourceStatus' "UNAVAILABLE"

pattern ResourceStatus_ZONAL_RESOURCE_INACCESSIBLE :: ResourceStatus
pattern ResourceStatus_ZONAL_RESOURCE_INACCESSIBLE = ResourceStatus' "ZONAL_RESOURCE_INACCESSIBLE"

{-# COMPLETE
  ResourceStatus_AVAILABLE,
  ResourceStatus_LIMIT_EXCEEDED,
  ResourceStatus_PENDING,
  ResourceStatus_UNAVAILABLE,
  ResourceStatus_ZONAL_RESOURCE_INACCESSIBLE,
  ResourceStatus'
  #-}
