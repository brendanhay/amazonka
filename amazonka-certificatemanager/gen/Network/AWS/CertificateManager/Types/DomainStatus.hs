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
-- Module      : Network.AWS.CertificateManager.Types.DomainStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.DomainStatus
  ( DomainStatus
      ( ..,
        DomainStatus_FAILED,
        DomainStatus_PENDING_VALIDATION,
        DomainStatus_SUCCESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DomainStatus = DomainStatus'
  { fromDomainStatus ::
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

pattern DomainStatus_FAILED :: DomainStatus
pattern DomainStatus_FAILED = DomainStatus' "FAILED"

pattern DomainStatus_PENDING_VALIDATION :: DomainStatus
pattern DomainStatus_PENDING_VALIDATION = DomainStatus' "PENDING_VALIDATION"

pattern DomainStatus_SUCCESS :: DomainStatus
pattern DomainStatus_SUCCESS = DomainStatus' "SUCCESS"

{-# COMPLETE
  DomainStatus_FAILED,
  DomainStatus_PENDING_VALIDATION,
  DomainStatus_SUCCESS,
  DomainStatus'
  #-}
