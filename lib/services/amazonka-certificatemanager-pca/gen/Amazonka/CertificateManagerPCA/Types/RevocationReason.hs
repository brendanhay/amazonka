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
-- Module      : Amazonka.CertificateManagerPCA.Types.RevocationReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.RevocationReason
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
