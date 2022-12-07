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
-- Module      : Amazonka.AppStream.Types.CertificateBasedAuthStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.CertificateBasedAuthStatus
  ( CertificateBasedAuthStatus
      ( ..,
        CertificateBasedAuthStatus_DISABLED,
        CertificateBasedAuthStatus_ENABLED,
        CertificateBasedAuthStatus_ENABLED_NO_DIRECTORY_LOGIN_FALLBACK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CertificateBasedAuthStatus = CertificateBasedAuthStatus'
  { fromCertificateBasedAuthStatus ::
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

pattern CertificateBasedAuthStatus_DISABLED :: CertificateBasedAuthStatus
pattern CertificateBasedAuthStatus_DISABLED = CertificateBasedAuthStatus' "DISABLED"

pattern CertificateBasedAuthStatus_ENABLED :: CertificateBasedAuthStatus
pattern CertificateBasedAuthStatus_ENABLED = CertificateBasedAuthStatus' "ENABLED"

pattern CertificateBasedAuthStatus_ENABLED_NO_DIRECTORY_LOGIN_FALLBACK :: CertificateBasedAuthStatus
pattern CertificateBasedAuthStatus_ENABLED_NO_DIRECTORY_LOGIN_FALLBACK = CertificateBasedAuthStatus' "ENABLED_NO_DIRECTORY_LOGIN_FALLBACK"

{-# COMPLETE
  CertificateBasedAuthStatus_DISABLED,
  CertificateBasedAuthStatus_ENABLED,
  CertificateBasedAuthStatus_ENABLED_NO_DIRECTORY_LOGIN_FALLBACK,
  CertificateBasedAuthStatus'
  #-}
