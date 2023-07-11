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
-- Module      : Amazonka.ServiceCatalog.Types.RequestStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.RequestStatus
  ( RequestStatus
      ( ..,
        RequestStatus_AVAILABLE,
        RequestStatus_CREATING,
        RequestStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RequestStatus = RequestStatus'
  { fromRequestStatus ::
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

pattern RequestStatus_AVAILABLE :: RequestStatus
pattern RequestStatus_AVAILABLE = RequestStatus' "AVAILABLE"

pattern RequestStatus_CREATING :: RequestStatus
pattern RequestStatus_CREATING = RequestStatus' "CREATING"

pattern RequestStatus_FAILED :: RequestStatus
pattern RequestStatus_FAILED = RequestStatus' "FAILED"

{-# COMPLETE
  RequestStatus_AVAILABLE,
  RequestStatus_CREATING,
  RequestStatus_FAILED,
  RequestStatus'
  #-}
