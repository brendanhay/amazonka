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
-- Module      : Amazonka.ServiceQuotas.Types.RequestStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.RequestStatus
  ( RequestStatus
      ( ..,
        RequestStatus_APPROVED,
        RequestStatus_CASE_CLOSED,
        RequestStatus_CASE_OPENED,
        RequestStatus_DENIED,
        RequestStatus_PENDING
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

pattern RequestStatus_APPROVED :: RequestStatus
pattern RequestStatus_APPROVED = RequestStatus' "APPROVED"

pattern RequestStatus_CASE_CLOSED :: RequestStatus
pattern RequestStatus_CASE_CLOSED = RequestStatus' "CASE_CLOSED"

pattern RequestStatus_CASE_OPENED :: RequestStatus
pattern RequestStatus_CASE_OPENED = RequestStatus' "CASE_OPENED"

pattern RequestStatus_DENIED :: RequestStatus
pattern RequestStatus_DENIED = RequestStatus' "DENIED"

pattern RequestStatus_PENDING :: RequestStatus
pattern RequestStatus_PENDING = RequestStatus' "PENDING"

{-# COMPLETE
  RequestStatus_APPROVED,
  RequestStatus_CASE_CLOSED,
  RequestStatus_CASE_OPENED,
  RequestStatus_DENIED,
  RequestStatus_PENDING,
  RequestStatus'
  #-}
