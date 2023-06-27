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
-- Module      : Amazonka.PaymentCryptographyData.Types.MacAlgorithm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.MacAlgorithm
  ( MacAlgorithm
      ( ..,
        MacAlgorithm_CMAC,
        MacAlgorithm_HMAC_SHA224,
        MacAlgorithm_HMAC_SHA256,
        MacAlgorithm_HMAC_SHA384,
        MacAlgorithm_HMAC_SHA512,
        MacAlgorithm_ISO9797_ALGORITHM1,
        MacAlgorithm_ISO9797_ALGORITHM3
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MacAlgorithm = MacAlgorithm'
  { fromMacAlgorithm ::
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

pattern MacAlgorithm_CMAC :: MacAlgorithm
pattern MacAlgorithm_CMAC = MacAlgorithm' "CMAC"

pattern MacAlgorithm_HMAC_SHA224 :: MacAlgorithm
pattern MacAlgorithm_HMAC_SHA224 = MacAlgorithm' "HMAC_SHA224"

pattern MacAlgorithm_HMAC_SHA256 :: MacAlgorithm
pattern MacAlgorithm_HMAC_SHA256 = MacAlgorithm' "HMAC_SHA256"

pattern MacAlgorithm_HMAC_SHA384 :: MacAlgorithm
pattern MacAlgorithm_HMAC_SHA384 = MacAlgorithm' "HMAC_SHA384"

pattern MacAlgorithm_HMAC_SHA512 :: MacAlgorithm
pattern MacAlgorithm_HMAC_SHA512 = MacAlgorithm' "HMAC_SHA512"

pattern MacAlgorithm_ISO9797_ALGORITHM1 :: MacAlgorithm
pattern MacAlgorithm_ISO9797_ALGORITHM1 = MacAlgorithm' "ISO9797_ALGORITHM1"

pattern MacAlgorithm_ISO9797_ALGORITHM3 :: MacAlgorithm
pattern MacAlgorithm_ISO9797_ALGORITHM3 = MacAlgorithm' "ISO9797_ALGORITHM3"

{-# COMPLETE
  MacAlgorithm_CMAC,
  MacAlgorithm_HMAC_SHA224,
  MacAlgorithm_HMAC_SHA256,
  MacAlgorithm_HMAC_SHA384,
  MacAlgorithm_HMAC_SHA512,
  MacAlgorithm_ISO9797_ALGORITHM1,
  MacAlgorithm_ISO9797_ALGORITHM3,
  MacAlgorithm'
  #-}
