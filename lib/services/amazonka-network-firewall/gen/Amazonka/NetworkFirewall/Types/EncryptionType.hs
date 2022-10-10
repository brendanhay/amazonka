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
-- Module      : Amazonka.NetworkFirewall.Types.EncryptionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.EncryptionType
  ( EncryptionType
      ( ..,
        EncryptionType_AWS_OWNED_KMS_KEY,
        EncryptionType_CUSTOMER_KMS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EncryptionType = EncryptionType'
  { fromEncryptionType ::
      Core.Text
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

pattern EncryptionType_AWS_OWNED_KMS_KEY :: EncryptionType
pattern EncryptionType_AWS_OWNED_KMS_KEY = EncryptionType' "AWS_OWNED_KMS_KEY"

pattern EncryptionType_CUSTOMER_KMS :: EncryptionType
pattern EncryptionType_CUSTOMER_KMS = EncryptionType' "CUSTOMER_KMS"

{-# COMPLETE
  EncryptionType_AWS_OWNED_KMS_KEY,
  EncryptionType_CUSTOMER_KMS,
  EncryptionType'
  #-}
