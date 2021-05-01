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
-- Module      : Network.AWS.MediaConvert.Types.HlsEncryptionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsEncryptionType
  ( HlsEncryptionType
      ( ..,
        HlsEncryptionType_AES128,
        HlsEncryptionType_SAMPLE_AES
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Encrypts the segments with the given encryption scheme. Leave blank to
-- disable. Selecting \'Disabled\' in the web interface also disables
-- encryption.
newtype HlsEncryptionType = HlsEncryptionType'
  { fromHlsEncryptionType ::
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

pattern HlsEncryptionType_AES128 :: HlsEncryptionType
pattern HlsEncryptionType_AES128 = HlsEncryptionType' "AES128"

pattern HlsEncryptionType_SAMPLE_AES :: HlsEncryptionType
pattern HlsEncryptionType_SAMPLE_AES = HlsEncryptionType' "SAMPLE_AES"

{-# COMPLETE
  HlsEncryptionType_AES128,
  HlsEncryptionType_SAMPLE_AES,
  HlsEncryptionType'
  #-}
