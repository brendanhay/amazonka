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
-- Module      : Network.AWS.CertificateManager.Types.RenewalStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.RenewalStatus
  ( RenewalStatus
      ( ..,
        RenewalStatus_FAILED,
        RenewalStatus_PENDING_AUTO_RENEWAL,
        RenewalStatus_PENDING_VALIDATION,
        RenewalStatus_SUCCESS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RenewalStatus = RenewalStatus'
  { fromRenewalStatus ::
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

pattern RenewalStatus_FAILED :: RenewalStatus
pattern RenewalStatus_FAILED = RenewalStatus' "FAILED"

pattern RenewalStatus_PENDING_AUTO_RENEWAL :: RenewalStatus
pattern RenewalStatus_PENDING_AUTO_RENEWAL = RenewalStatus' "PENDING_AUTO_RENEWAL"

pattern RenewalStatus_PENDING_VALIDATION :: RenewalStatus
pattern RenewalStatus_PENDING_VALIDATION = RenewalStatus' "PENDING_VALIDATION"

pattern RenewalStatus_SUCCESS :: RenewalStatus
pattern RenewalStatus_SUCCESS = RenewalStatus' "SUCCESS"

{-# COMPLETE
  RenewalStatus_FAILED,
  RenewalStatus_PENDING_AUTO_RENEWAL,
  RenewalStatus_PENDING_VALIDATION,
  RenewalStatus_SUCCESS,
  RenewalStatus'
  #-}
