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
-- Module      : Amazonka.MediaPackageV2.Types.TsEncryptionMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.TsEncryptionMethod
  ( TsEncryptionMethod
      ( ..,
        TsEncryptionMethod_AES_128,
        TsEncryptionMethod_SAMPLE_AES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TsEncryptionMethod = TsEncryptionMethod'
  { fromTsEncryptionMethod ::
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

pattern TsEncryptionMethod_AES_128 :: TsEncryptionMethod
pattern TsEncryptionMethod_AES_128 = TsEncryptionMethod' "AES_128"

pattern TsEncryptionMethod_SAMPLE_AES :: TsEncryptionMethod
pattern TsEncryptionMethod_SAMPLE_AES = TsEncryptionMethod' "SAMPLE_AES"

{-# COMPLETE
  TsEncryptionMethod_AES_128,
  TsEncryptionMethod_SAMPLE_AES,
  TsEncryptionMethod'
  #-}
