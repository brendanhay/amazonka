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
-- Module      : Amazonka.PaymentCryptography.Types.KeyOrigin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.KeyOrigin
  ( KeyOrigin
      ( ..,
        KeyOrigin_AWS_PAYMENT_CRYPTOGRAPHY,
        KeyOrigin_EXTERNAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the source of a key
newtype KeyOrigin = KeyOrigin'
  { fromKeyOrigin ::
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

pattern KeyOrigin_AWS_PAYMENT_CRYPTOGRAPHY :: KeyOrigin
pattern KeyOrigin_AWS_PAYMENT_CRYPTOGRAPHY = KeyOrigin' "AWS_PAYMENT_CRYPTOGRAPHY"

pattern KeyOrigin_EXTERNAL :: KeyOrigin
pattern KeyOrigin_EXTERNAL = KeyOrigin' "EXTERNAL"

{-# COMPLETE
  KeyOrigin_AWS_PAYMENT_CRYPTOGRAPHY,
  KeyOrigin_EXTERNAL,
  KeyOrigin'
  #-}
