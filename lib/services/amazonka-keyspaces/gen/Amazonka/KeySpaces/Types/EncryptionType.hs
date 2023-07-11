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
-- Module      : Amazonka.KeySpaces.Types.EncryptionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.EncryptionType
  ( EncryptionType
      ( ..,
        EncryptionType_AWS_OWNED_KMS_KEY,
        EncryptionType_CUSTOMER_MANAGED_KMS_KEY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

pattern EncryptionType_AWS_OWNED_KMS_KEY :: EncryptionType
pattern EncryptionType_AWS_OWNED_KMS_KEY = EncryptionType' "AWS_OWNED_KMS_KEY"

pattern EncryptionType_CUSTOMER_MANAGED_KMS_KEY :: EncryptionType
pattern EncryptionType_CUSTOMER_MANAGED_KMS_KEY = EncryptionType' "CUSTOMER_MANAGED_KMS_KEY"

{-# COMPLETE
  EncryptionType_AWS_OWNED_KMS_KEY,
  EncryptionType_CUSTOMER_MANAGED_KMS_KEY,
  EncryptionType'
  #-}
