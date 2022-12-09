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
-- Module      : Amazonka.MacieV2.Types.EncryptionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.EncryptionType
  ( EncryptionType
      ( ..,
        EncryptionType_AES256,
        EncryptionType_Aws_kms,
        EncryptionType_NONE,
        EncryptionType_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of server-side encryption that\'s used to encrypt an S3 object
-- or objects in an S3 bucket. Possible values are:
newtype EncryptionType = EncryptionType'
  { fromEncryptionType ::
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

pattern EncryptionType_AES256 :: EncryptionType
pattern EncryptionType_AES256 = EncryptionType' "AES256"

pattern EncryptionType_Aws_kms :: EncryptionType
pattern EncryptionType_Aws_kms = EncryptionType' "aws:kms"

pattern EncryptionType_NONE :: EncryptionType
pattern EncryptionType_NONE = EncryptionType' "NONE"

pattern EncryptionType_UNKNOWN :: EncryptionType
pattern EncryptionType_UNKNOWN = EncryptionType' "UNKNOWN"

{-# COMPLETE
  EncryptionType_AES256,
  EncryptionType_Aws_kms,
  EncryptionType_NONE,
  EncryptionType_UNKNOWN,
  EncryptionType'
  #-}
