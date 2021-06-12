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
-- Module      : Network.AWS.CertificateManagerPCA.Types.RevocationReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.RevocationReason
  ( RevocationReason
      ( ..,
        RevocationReason_AFFILIATION_CHANGED,
        RevocationReason_A_A_COMPROMISE,
        RevocationReason_CERTIFICATE_AUTHORITY_COMPROMISE,
        RevocationReason_CESSATION_OF_OPERATION,
        RevocationReason_KEY_COMPROMISE,
        RevocationReason_PRIVILEGE_WITHDRAWN,
        RevocationReason_SUPERSEDED,
        RevocationReason_UNSPECIFIED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RevocationReason = RevocationReason'
  { fromRevocationReason ::
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

pattern RevocationReason_AFFILIATION_CHANGED :: RevocationReason
pattern RevocationReason_AFFILIATION_CHANGED = RevocationReason' "AFFILIATION_CHANGED"

pattern RevocationReason_A_A_COMPROMISE :: RevocationReason
pattern RevocationReason_A_A_COMPROMISE = RevocationReason' "A_A_COMPROMISE"

pattern RevocationReason_CERTIFICATE_AUTHORITY_COMPROMISE :: RevocationReason
pattern RevocationReason_CERTIFICATE_AUTHORITY_COMPROMISE = RevocationReason' "CERTIFICATE_AUTHORITY_COMPROMISE"

pattern RevocationReason_CESSATION_OF_OPERATION :: RevocationReason
pattern RevocationReason_CESSATION_OF_OPERATION = RevocationReason' "CESSATION_OF_OPERATION"

pattern RevocationReason_KEY_COMPROMISE :: RevocationReason
pattern RevocationReason_KEY_COMPROMISE = RevocationReason' "KEY_COMPROMISE"

pattern RevocationReason_PRIVILEGE_WITHDRAWN :: RevocationReason
pattern RevocationReason_PRIVILEGE_WITHDRAWN = RevocationReason' "PRIVILEGE_WITHDRAWN"

pattern RevocationReason_SUPERSEDED :: RevocationReason
pattern RevocationReason_SUPERSEDED = RevocationReason' "SUPERSEDED"

pattern RevocationReason_UNSPECIFIED :: RevocationReason
pattern RevocationReason_UNSPECIFIED = RevocationReason' "UNSPECIFIED"

{-# COMPLETE
  RevocationReason_AFFILIATION_CHANGED,
  RevocationReason_A_A_COMPROMISE,
  RevocationReason_CERTIFICATE_AUTHORITY_COMPROMISE,
  RevocationReason_CESSATION_OF_OPERATION,
  RevocationReason_KEY_COMPROMISE,
  RevocationReason_PRIVILEGE_WITHDRAWN,
  RevocationReason_SUPERSEDED,
  RevocationReason_UNSPECIFIED,
  RevocationReason'
  #-}
