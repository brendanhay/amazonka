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

import qualified Network.AWS.Core as Core

newtype EncryptionOption = EncryptionOption'
  { fromEncryptionOption ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
