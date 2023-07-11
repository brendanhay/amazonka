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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KeyStorageSecurityStandard = KeyStorageSecurityStandard'
  { fromKeyStorageSecurityStandard ::
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

pattern KeyStorageSecurityStandard_FIPS_140_2_LEVEL_2_OR_HIGHER :: KeyStorageSecurityStandard
pattern KeyStorageSecurityStandard_FIPS_140_2_LEVEL_2_OR_HIGHER = KeyStorageSecurityStandard' "FIPS_140_2_LEVEL_2_OR_HIGHER"

pattern KeyStorageSecurityStandard_FIPS_140_2_LEVEL_3_OR_HIGHER :: KeyStorageSecurityStandard
pattern KeyStorageSecurityStandard_FIPS_140_2_LEVEL_3_OR_HIGHER = KeyStorageSecurityStandard' "FIPS_140_2_LEVEL_3_OR_HIGHER"

{-# COMPLETE
  KeyStorageSecurityStandard_FIPS_140_2_LEVEL_2_OR_HIGHER,
  KeyStorageSecurityStandard_FIPS_140_2_LEVEL_3_OR_HIGHER,
  KeyStorageSecurityStandard'
  #-}
