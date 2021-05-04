{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafEncryptionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafEncryptionType
  ( CmafEncryptionType
      ( ..,
        CmafEncryptionType_AES_CTR,
        CmafEncryptionType_SAMPLE_AES
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specify the encryption scheme that you want the service to use when
-- encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or
-- AES_CTR (AES-CTR).
newtype CmafEncryptionType = CmafEncryptionType'
  { fromCmafEncryptionType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern CmafEncryptionType_AES_CTR :: CmafEncryptionType
pattern CmafEncryptionType_AES_CTR = CmafEncryptionType' "AES_CTR"

pattern CmafEncryptionType_SAMPLE_AES :: CmafEncryptionType
pattern CmafEncryptionType_SAMPLE_AES = CmafEncryptionType' "SAMPLE_AES"

{-# COMPLETE
  CmafEncryptionType_AES_CTR,
  CmafEncryptionType_SAMPLE_AES,
  CmafEncryptionType'
  #-}
