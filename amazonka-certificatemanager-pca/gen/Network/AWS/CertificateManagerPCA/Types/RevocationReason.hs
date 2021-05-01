{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype RevocationReason = RevocationReason'
  { fromRevocationReason ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
