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
-- Module      : Amazonka.PaymentCryptography.Types.KeyClass
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.KeyClass
  ( KeyClass
      ( ..,
        KeyClass_ASYMMETRIC_KEY_PAIR,
        KeyClass_PRIVATE_KEY,
        KeyClass_PUBLIC_KEY,
        KeyClass_SYMMETRIC_KEY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KeyClass = KeyClass'
  { fromKeyClass ::
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

pattern KeyClass_ASYMMETRIC_KEY_PAIR :: KeyClass
pattern KeyClass_ASYMMETRIC_KEY_PAIR = KeyClass' "ASYMMETRIC_KEY_PAIR"

pattern KeyClass_PRIVATE_KEY :: KeyClass
pattern KeyClass_PRIVATE_KEY = KeyClass' "PRIVATE_KEY"

pattern KeyClass_PUBLIC_KEY :: KeyClass
pattern KeyClass_PUBLIC_KEY = KeyClass' "PUBLIC_KEY"

pattern KeyClass_SYMMETRIC_KEY :: KeyClass
pattern KeyClass_SYMMETRIC_KEY = KeyClass' "SYMMETRIC_KEY"

{-# COMPLETE
  KeyClass_ASYMMETRIC_KEY_PAIR,
  KeyClass_PRIVATE_KEY,
  KeyClass_PUBLIC_KEY,
  KeyClass_SYMMETRIC_KEY,
  KeyClass'
  #-}
