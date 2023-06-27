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
-- Module      : Amazonka.PaymentCryptography.Types.KeyCheckValueAlgorithm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.KeyCheckValueAlgorithm
  ( KeyCheckValueAlgorithm
      ( ..,
        KeyCheckValueAlgorithm_ANSI_X9_24,
        KeyCheckValueAlgorithm_CMAC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KeyCheckValueAlgorithm = KeyCheckValueAlgorithm'
  { fromKeyCheckValueAlgorithm ::
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

pattern KeyCheckValueAlgorithm_ANSI_X9_24 :: KeyCheckValueAlgorithm
pattern KeyCheckValueAlgorithm_ANSI_X9_24 = KeyCheckValueAlgorithm' "ANSI_X9_24"

pattern KeyCheckValueAlgorithm_CMAC :: KeyCheckValueAlgorithm
pattern KeyCheckValueAlgorithm_CMAC = KeyCheckValueAlgorithm' "CMAC"

{-# COMPLETE
  KeyCheckValueAlgorithm_ANSI_X9_24,
  KeyCheckValueAlgorithm_CMAC,
  KeyCheckValueAlgorithm'
  #-}
