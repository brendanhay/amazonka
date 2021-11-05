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
-- Module      : Network.AWS.Nimble.Types.StudioEncryptionConfigurationKeyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Nimble.Types.StudioEncryptionConfigurationKeyType
  ( StudioEncryptionConfigurationKeyType
      ( ..,
        StudioEncryptionConfigurationKeyType_AWS_OWNED_KEY,
        StudioEncryptionConfigurationKeyType_CUSTOMER_MANAGED_KEY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The type of KMS key that is used to encrypt studio data.
newtype StudioEncryptionConfigurationKeyType = StudioEncryptionConfigurationKeyType'
  { fromStudioEncryptionConfigurationKeyType ::
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

pattern StudioEncryptionConfigurationKeyType_AWS_OWNED_KEY :: StudioEncryptionConfigurationKeyType
pattern StudioEncryptionConfigurationKeyType_AWS_OWNED_KEY = StudioEncryptionConfigurationKeyType' "AWS_OWNED_KEY"

pattern StudioEncryptionConfigurationKeyType_CUSTOMER_MANAGED_KEY :: StudioEncryptionConfigurationKeyType
pattern StudioEncryptionConfigurationKeyType_CUSTOMER_MANAGED_KEY = StudioEncryptionConfigurationKeyType' "CUSTOMER_MANAGED_KEY"

{-# COMPLETE
  StudioEncryptionConfigurationKeyType_AWS_OWNED_KEY,
  StudioEncryptionConfigurationKeyType_CUSTOMER_MANAGED_KEY,
  StudioEncryptionConfigurationKeyType'
  #-}
