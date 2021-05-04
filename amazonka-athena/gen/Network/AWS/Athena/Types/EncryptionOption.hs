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
-- Module      : Network.AWS.Athena.Types.EncryptionOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.EncryptionOption
  ( EncryptionOption
      ( ..,
        EncryptionOption_CSE_KMS,
        EncryptionOption_SSE_KMS,
        EncryptionOption_SSE_S3
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype EncryptionOption = EncryptionOption'
  { fromEncryptionOption ::
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

pattern EncryptionOption_CSE_KMS :: EncryptionOption
pattern EncryptionOption_CSE_KMS = EncryptionOption' "CSE_KMS"

pattern EncryptionOption_SSE_KMS :: EncryptionOption
pattern EncryptionOption_SSE_KMS = EncryptionOption' "SSE_KMS"

pattern EncryptionOption_SSE_S3 :: EncryptionOption
pattern EncryptionOption_SSE_S3 = EncryptionOption' "SSE_S3"

{-# COMPLETE
  EncryptionOption_CSE_KMS,
  EncryptionOption_SSE_KMS,
  EncryptionOption_SSE_S3,
  EncryptionOption'
  #-}
