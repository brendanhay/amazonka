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
-- Module      : Network.AWS.MediaPackage.Types.EncryptionMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.EncryptionMethod
  ( EncryptionMethod
      ( ..,
        EncryptionMethod_AES_128,
        EncryptionMethod_SAMPLE_AES
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EncryptionMethod = EncryptionMethod'
  { fromEncryptionMethod ::
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

pattern EncryptionMethod_AES_128 :: EncryptionMethod
pattern EncryptionMethod_AES_128 = EncryptionMethod' "AES_128"

pattern EncryptionMethod_SAMPLE_AES :: EncryptionMethod
pattern EncryptionMethod_SAMPLE_AES = EncryptionMethod' "SAMPLE_AES"

{-# COMPLETE
  EncryptionMethod_AES_128,
  EncryptionMethod_SAMPLE_AES,
  EncryptionMethod'
  #-}
