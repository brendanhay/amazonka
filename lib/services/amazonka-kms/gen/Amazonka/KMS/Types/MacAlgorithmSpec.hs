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
-- Module      : Amazonka.KMS.Types.MacAlgorithmSpec
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.MacAlgorithmSpec
  ( MacAlgorithmSpec
      ( ..,
        MacAlgorithmSpec_HMAC_SHA_224,
        MacAlgorithmSpec_HMAC_SHA_256,
        MacAlgorithmSpec_HMAC_SHA_384,
        MacAlgorithmSpec_HMAC_SHA_512
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MacAlgorithmSpec = MacAlgorithmSpec'
  { fromMacAlgorithmSpec ::
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

pattern MacAlgorithmSpec_HMAC_SHA_224 :: MacAlgorithmSpec
pattern MacAlgorithmSpec_HMAC_SHA_224 = MacAlgorithmSpec' "HMAC_SHA_224"

pattern MacAlgorithmSpec_HMAC_SHA_256 :: MacAlgorithmSpec
pattern MacAlgorithmSpec_HMAC_SHA_256 = MacAlgorithmSpec' "HMAC_SHA_256"

pattern MacAlgorithmSpec_HMAC_SHA_384 :: MacAlgorithmSpec
pattern MacAlgorithmSpec_HMAC_SHA_384 = MacAlgorithmSpec' "HMAC_SHA_384"

pattern MacAlgorithmSpec_HMAC_SHA_512 :: MacAlgorithmSpec
pattern MacAlgorithmSpec_HMAC_SHA_512 = MacAlgorithmSpec' "HMAC_SHA_512"

{-# COMPLETE
  MacAlgorithmSpec_HMAC_SHA_224,
  MacAlgorithmSpec_HMAC_SHA_256,
  MacAlgorithmSpec_HMAC_SHA_384,
  MacAlgorithmSpec_HMAC_SHA_512,
  MacAlgorithmSpec'
  #-}
