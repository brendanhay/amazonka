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

import qualified Network.AWS.Prelude as Prelude

newtype EncryptionModeValue = EncryptionModeValue'
  { fromEncryptionModeValue ::
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

pattern EncryptionModeValue_Sse_kms :: EncryptionModeValue
pattern EncryptionModeValue_Sse_kms = EncryptionModeValue' "sse-kms"

pattern EncryptionModeValue_Sse_s3 :: EncryptionModeValue
pattern EncryptionModeValue_Sse_s3 = EncryptionModeValue' "sse-s3"

{-# COMPLETE
  EncryptionModeValue_Sse_kms,
  EncryptionModeValue_Sse_s3,
  EncryptionModeValue'
  #-}
