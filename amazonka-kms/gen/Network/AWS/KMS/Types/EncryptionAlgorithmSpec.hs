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
-- Module      : Network.AWS.KMS.Types.EncryptionAlgorithmSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.EncryptionAlgorithmSpec
  ( EncryptionAlgorithmSpec
      ( ..,
        EncryptionAlgorithmSpec_RSAES_OAEP_SHA_1,
        EncryptionAlgorithmSpec_RSAES_OAEP_SHA_256,
        EncryptionAlgorithmSpec_SYMMETRIC_DEFAULT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EncryptionAlgorithmSpec = EncryptionAlgorithmSpec'
  { fromEncryptionAlgorithmSpec ::
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

pattern EncryptionAlgorithmSpec_RSAES_OAEP_SHA_1 :: EncryptionAlgorithmSpec
pattern EncryptionAlgorithmSpec_RSAES_OAEP_SHA_1 = EncryptionAlgorithmSpec' "RSAES_OAEP_SHA_1"

pattern EncryptionAlgorithmSpec_RSAES_OAEP_SHA_256 :: EncryptionAlgorithmSpec
pattern EncryptionAlgorithmSpec_RSAES_OAEP_SHA_256 = EncryptionAlgorithmSpec' "RSAES_OAEP_SHA_256"

pattern EncryptionAlgorithmSpec_SYMMETRIC_DEFAULT :: EncryptionAlgorithmSpec
pattern EncryptionAlgorithmSpec_SYMMETRIC_DEFAULT = EncryptionAlgorithmSpec' "SYMMETRIC_DEFAULT"

{-# COMPLETE
  EncryptionAlgorithmSpec_RSAES_OAEP_SHA_1,
  EncryptionAlgorithmSpec_RSAES_OAEP_SHA_256,
  EncryptionAlgorithmSpec_SYMMETRIC_DEFAULT,
  EncryptionAlgorithmSpec'
  #-}
