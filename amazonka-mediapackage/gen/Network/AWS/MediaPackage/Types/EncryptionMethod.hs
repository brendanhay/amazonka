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

import qualified Network.AWS.Prelude as Prelude

newtype EncryptionMethod = EncryptionMethod'
  { fromEncryptionMethod ::
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

pattern EncryptionMethod_AES_128 :: EncryptionMethod
pattern EncryptionMethod_AES_128 = EncryptionMethod' "AES_128"

pattern EncryptionMethod_SAMPLE_AES :: EncryptionMethod
pattern EncryptionMethod_SAMPLE_AES = EncryptionMethod' "SAMPLE_AES"

{-# COMPLETE
  EncryptionMethod_AES_128,
  EncryptionMethod_SAMPLE_AES,
  EncryptionMethod'
  #-}
