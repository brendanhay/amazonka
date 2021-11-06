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
-- Module      : Amazonka.CertificateManagerPCA.Types.KeyStorageSecurityStandard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.KeyStorageSecurityStandard
  ( KeyStorageSecurityStandard
      ( ..,
        KeyStorageSecurityStandard_FIPS_140_2_LEVEL_2_OR_HIGHER,
        KeyStorageSecurityStandard_FIPS_140_2_LEVEL_3_OR_HIGHER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype KeyStorageSecurityStandard = KeyStorageSecurityStandard'
  { fromKeyStorageSecurityStandard ::
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

pattern KeyStorageSecurityStandard_FIPS_140_2_LEVEL_2_OR_HIGHER :: KeyStorageSecurityStandard
pattern KeyStorageSecurityStandard_FIPS_140_2_LEVEL_2_OR_HIGHER = KeyStorageSecurityStandard' "FIPS_140_2_LEVEL_2_OR_HIGHER"

pattern KeyStorageSecurityStandard_FIPS_140_2_LEVEL_3_OR_HIGHER :: KeyStorageSecurityStandard
pattern KeyStorageSecurityStandard_FIPS_140_2_LEVEL_3_OR_HIGHER = KeyStorageSecurityStandard' "FIPS_140_2_LEVEL_3_OR_HIGHER"

{-# COMPLETE
  KeyStorageSecurityStandard_FIPS_140_2_LEVEL_2_OR_HIGHER,
  KeyStorageSecurityStandard_FIPS_140_2_LEVEL_3_OR_HIGHER,
  KeyStorageSecurityStandard'
  #-}
