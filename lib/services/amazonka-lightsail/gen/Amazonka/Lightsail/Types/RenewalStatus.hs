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
-- Module      : Amazonka.Lightsail.Types.RenewalStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RenewalStatus
  ( RenewalStatus
      ( ..,
        RenewalStatus_Failed,
        RenewalStatus_PendingAutoRenewal,
        RenewalStatus_PendingValidation,
        RenewalStatus_Success
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RenewalStatus = RenewalStatus'
  { fromRenewalStatus ::
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

pattern RenewalStatus_Failed :: RenewalStatus
pattern RenewalStatus_Failed = RenewalStatus' "Failed"

pattern RenewalStatus_PendingAutoRenewal :: RenewalStatus
pattern RenewalStatus_PendingAutoRenewal = RenewalStatus' "PendingAutoRenewal"

pattern RenewalStatus_PendingValidation :: RenewalStatus
pattern RenewalStatus_PendingValidation = RenewalStatus' "PendingValidation"

pattern RenewalStatus_Success :: RenewalStatus
pattern RenewalStatus_Success = RenewalStatus' "Success"

{-# COMPLETE
  RenewalStatus_Failed,
  RenewalStatus_PendingAutoRenewal,
  RenewalStatus_PendingValidation,
  RenewalStatus_Success,
  RenewalStatus'
  #-}
