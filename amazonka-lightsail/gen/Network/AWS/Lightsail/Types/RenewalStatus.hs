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
-- Module      : Network.AWS.Lightsail.Types.RenewalStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RenewalStatus
  ( RenewalStatus
      ( ..,
        RenewalStatus_Failed,
        RenewalStatus_PendingAutoRenewal,
        RenewalStatus_PendingValidation,
        RenewalStatus_Success
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RenewalStatus = RenewalStatus'
  { fromRenewalStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
