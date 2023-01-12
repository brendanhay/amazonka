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
-- Module      : Amazonka.Nimble.Types.StudioEncryptionConfigurationKeyType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioEncryptionConfigurationKeyType
  ( StudioEncryptionConfigurationKeyType
      ( ..,
        StudioEncryptionConfigurationKeyType_AWS_OWNED_KEY,
        StudioEncryptionConfigurationKeyType_CUSTOMER_MANAGED_KEY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of KMS key that is used to encrypt studio data.
newtype StudioEncryptionConfigurationKeyType = StudioEncryptionConfigurationKeyType'
  { fromStudioEncryptionConfigurationKeyType ::
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

pattern StudioEncryptionConfigurationKeyType_AWS_OWNED_KEY :: StudioEncryptionConfigurationKeyType
pattern StudioEncryptionConfigurationKeyType_AWS_OWNED_KEY = StudioEncryptionConfigurationKeyType' "AWS_OWNED_KEY"

pattern StudioEncryptionConfigurationKeyType_CUSTOMER_MANAGED_KEY :: StudioEncryptionConfigurationKeyType
pattern StudioEncryptionConfigurationKeyType_CUSTOMER_MANAGED_KEY = StudioEncryptionConfigurationKeyType' "CUSTOMER_MANAGED_KEY"

{-# COMPLETE
  StudioEncryptionConfigurationKeyType_AWS_OWNED_KEY,
  StudioEncryptionConfigurationKeyType_CUSTOMER_MANAGED_KEY,
  StudioEncryptionConfigurationKeyType'
  #-}
