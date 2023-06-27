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
-- Module      : Amazonka.PaymentCryptography.Types.KeyMaterialType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.KeyMaterialType
  ( KeyMaterialType
      ( ..,
        KeyMaterialType_ROOT_PUBLIC_KEY_CERTIFICATE,
        KeyMaterialType_TR31_KEY_BLOCK,
        KeyMaterialType_TR34_KEY_BLOCK,
        KeyMaterialType_TRUSTED_PUBLIC_KEY_CERTIFICATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KeyMaterialType = KeyMaterialType'
  { fromKeyMaterialType ::
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

pattern KeyMaterialType_ROOT_PUBLIC_KEY_CERTIFICATE :: KeyMaterialType
pattern KeyMaterialType_ROOT_PUBLIC_KEY_CERTIFICATE = KeyMaterialType' "ROOT_PUBLIC_KEY_CERTIFICATE"

pattern KeyMaterialType_TR31_KEY_BLOCK :: KeyMaterialType
pattern KeyMaterialType_TR31_KEY_BLOCK = KeyMaterialType' "TR31_KEY_BLOCK"

pattern KeyMaterialType_TR34_KEY_BLOCK :: KeyMaterialType
pattern KeyMaterialType_TR34_KEY_BLOCK = KeyMaterialType' "TR34_KEY_BLOCK"

pattern KeyMaterialType_TRUSTED_PUBLIC_KEY_CERTIFICATE :: KeyMaterialType
pattern KeyMaterialType_TRUSTED_PUBLIC_KEY_CERTIFICATE = KeyMaterialType' "TRUSTED_PUBLIC_KEY_CERTIFICATE"

{-# COMPLETE
  KeyMaterialType_ROOT_PUBLIC_KEY_CERTIFICATE,
  KeyMaterialType_TR31_KEY_BLOCK,
  KeyMaterialType_TR34_KEY_BLOCK,
  KeyMaterialType_TRUSTED_PUBLIC_KEY_CERTIFICATE,
  KeyMaterialType'
  #-}
