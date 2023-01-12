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
-- Module      : Amazonka.MediaPackage.Types.CmafEncryptionMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.CmafEncryptionMethod
  ( CmafEncryptionMethod
      ( ..,
        CmafEncryptionMethod_AES_CTR,
        CmafEncryptionMethod_SAMPLE_AES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The encryption method to use.
newtype CmafEncryptionMethod = CmafEncryptionMethod'
  { fromCmafEncryptionMethod ::
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

pattern CmafEncryptionMethod_AES_CTR :: CmafEncryptionMethod
pattern CmafEncryptionMethod_AES_CTR = CmafEncryptionMethod' "AES_CTR"

pattern CmafEncryptionMethod_SAMPLE_AES :: CmafEncryptionMethod
pattern CmafEncryptionMethod_SAMPLE_AES = CmafEncryptionMethod' "SAMPLE_AES"

{-# COMPLETE
  CmafEncryptionMethod_AES_CTR,
  CmafEncryptionMethod_SAMPLE_AES,
  CmafEncryptionMethod'
  #-}
