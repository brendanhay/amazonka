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
-- Module      : Network.AWS.IoTSiteWise.Types.EncryptionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Types.EncryptionType
  ( EncryptionType
      ( ..,
        EncryptionType_KMS_BASED_ENCRYPTION,
        EncryptionType_SITEWISE_DEFAULT_ENCRYPTION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

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

pattern EncryptionType_KMS_BASED_ENCRYPTION :: EncryptionType
pattern EncryptionType_KMS_BASED_ENCRYPTION = EncryptionType' "KMS_BASED_ENCRYPTION"

pattern EncryptionType_SITEWISE_DEFAULT_ENCRYPTION :: EncryptionType
pattern EncryptionType_SITEWISE_DEFAULT_ENCRYPTION = EncryptionType' "SITEWISE_DEFAULT_ENCRYPTION"

{-# COMPLETE
  EncryptionType_KMS_BASED_ENCRYPTION,
  EncryptionType_SITEWISE_DEFAULT_ENCRYPTION,
  EncryptionType'
  #-}
