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
-- Module      : Amazonka.ElastiCache.Types.ServiceUpdateStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.ServiceUpdateStatus
  ( ServiceUpdateStatus
      ( ..,
        ServiceUpdateStatus_Available,
        ServiceUpdateStatus_Cancelled,
        ServiceUpdateStatus_Expired
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceUpdateStatus = ServiceUpdateStatus'
  { fromServiceUpdateStatus ::
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

pattern ServiceUpdateStatus_Available :: ServiceUpdateStatus
pattern ServiceUpdateStatus_Available = ServiceUpdateStatus' "available"

pattern ServiceUpdateStatus_Cancelled :: ServiceUpdateStatus
pattern ServiceUpdateStatus_Cancelled = ServiceUpdateStatus' "cancelled"

pattern ServiceUpdateStatus_Expired :: ServiceUpdateStatus
pattern ServiceUpdateStatus_Expired = ServiceUpdateStatus' "expired"

{-# COMPLETE
  ServiceUpdateStatus_Available,
  ServiceUpdateStatus_Cancelled,
  ServiceUpdateStatus_Expired,
  ServiceUpdateStatus'
  #-}
