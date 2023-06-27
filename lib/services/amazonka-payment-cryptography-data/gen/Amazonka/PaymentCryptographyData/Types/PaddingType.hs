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
-- Module      : Amazonka.PaymentCryptographyData.Types.PaddingType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.PaddingType
  ( PaddingType
      ( ..,
        PaddingType_OAEP_SHA1,
        PaddingType_OAEP_SHA256,
        PaddingType_OAEP_SHA512,
        PaddingType_PKCS1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PaddingType = PaddingType'
  { fromPaddingType ::
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

pattern PaddingType_OAEP_SHA1 :: PaddingType
pattern PaddingType_OAEP_SHA1 = PaddingType' "OAEP_SHA1"

pattern PaddingType_OAEP_SHA256 :: PaddingType
pattern PaddingType_OAEP_SHA256 = PaddingType' "OAEP_SHA256"

pattern PaddingType_OAEP_SHA512 :: PaddingType
pattern PaddingType_OAEP_SHA512 = PaddingType' "OAEP_SHA512"

pattern PaddingType_PKCS1 :: PaddingType
pattern PaddingType_PKCS1 = PaddingType' "PKCS1"

{-# COMPLETE
  PaddingType_OAEP_SHA1,
  PaddingType_OAEP_SHA256,
  PaddingType_OAEP_SHA512,
  PaddingType_PKCS1,
  PaddingType'
  #-}
