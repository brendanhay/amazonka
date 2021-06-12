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
-- Module      : Network.AWS.DMS.Types.EncryptionModeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.EncryptionModeValue
  ( EncryptionModeValue
      ( ..,
        EncryptionModeValue_Sse_kms,
        EncryptionModeValue_Sse_s3
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EncryptionModeValue = EncryptionModeValue'
  { fromEncryptionModeValue ::
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

pattern EncryptionModeValue_Sse_kms :: EncryptionModeValue
pattern EncryptionModeValue_Sse_kms = EncryptionModeValue' "sse-kms"

pattern EncryptionModeValue_Sse_s3 :: EncryptionModeValue
pattern EncryptionModeValue_Sse_s3 = EncryptionModeValue' "sse-s3"

{-# COMPLETE
  EncryptionModeValue_Sse_kms,
  EncryptionModeValue_Sse_s3,
  EncryptionModeValue'
  #-}
