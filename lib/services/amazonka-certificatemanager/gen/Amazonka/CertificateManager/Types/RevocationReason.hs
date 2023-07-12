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
-- Module      : Amazonka.CertificateManager.Types.RevocationReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.RevocationReason
  ( RevocationReason
      ( ..,
        RevocationReason_AFFILIATION_CHANGED,
        RevocationReason_A_A_COMPROMISE,
        RevocationReason_CA_COMPROMISE,
        RevocationReason_CERTIFICATE_HOLD,
        RevocationReason_CESSATION_OF_OPERATION,
        RevocationReason_KEY_COMPROMISE,
        RevocationReason_PRIVILEGE_WITHDRAWN,
        RevocationReason_REMOVE_FROM_CRL,
        RevocationReason_SUPERCEDED,
        RevocationReason_UNSPECIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RevocationReason = RevocationReason'
  { fromRevocationReason ::
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

pattern RevocationReason_AFFILIATION_CHANGED :: RevocationReason
pattern RevocationReason_AFFILIATION_CHANGED = RevocationReason' "AFFILIATION_CHANGED"

pattern RevocationReason_A_A_COMPROMISE :: RevocationReason
pattern RevocationReason_A_A_COMPROMISE = RevocationReason' "A_A_COMPROMISE"

pattern RevocationReason_CA_COMPROMISE :: RevocationReason
pattern RevocationReason_CA_COMPROMISE = RevocationReason' "CA_COMPROMISE"

pattern RevocationReason_CERTIFICATE_HOLD :: RevocationReason
pattern RevocationReason_CERTIFICATE_HOLD = RevocationReason' "CERTIFICATE_HOLD"

pattern RevocationReason_CESSATION_OF_OPERATION :: RevocationReason
pattern RevocationReason_CESSATION_OF_OPERATION = RevocationReason' "CESSATION_OF_OPERATION"

pattern RevocationReason_KEY_COMPROMISE :: RevocationReason
pattern RevocationReason_KEY_COMPROMISE = RevocationReason' "KEY_COMPROMISE"

pattern RevocationReason_PRIVILEGE_WITHDRAWN :: RevocationReason
pattern RevocationReason_PRIVILEGE_WITHDRAWN = RevocationReason' "PRIVILEGE_WITHDRAWN"

pattern RevocationReason_REMOVE_FROM_CRL :: RevocationReason
pattern RevocationReason_REMOVE_FROM_CRL = RevocationReason' "REMOVE_FROM_CRL"

pattern RevocationReason_SUPERCEDED :: RevocationReason
pattern RevocationReason_SUPERCEDED = RevocationReason' "SUPERCEDED"

pattern RevocationReason_UNSPECIFIED :: RevocationReason
pattern RevocationReason_UNSPECIFIED = RevocationReason' "UNSPECIFIED"

{-# COMPLETE
  RevocationReason_AFFILIATION_CHANGED,
  RevocationReason_A_A_COMPROMISE,
  RevocationReason_CA_COMPROMISE,
  RevocationReason_CERTIFICATE_HOLD,
  RevocationReason_CESSATION_OF_OPERATION,
  RevocationReason_KEY_COMPROMISE,
  RevocationReason_PRIVILEGE_WITHDRAWN,
  RevocationReason_REMOVE_FROM_CRL,
  RevocationReason_SUPERCEDED,
  RevocationReason_UNSPECIFIED,
  RevocationReason'
  #-}
