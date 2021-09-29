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
-- Module      : Network.AWS.QLDB.Types.EncryptionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Types.EncryptionStatus
  ( EncryptionStatus
      ( ..,
        EncryptionStatus_ENABLED,
        EncryptionStatus_KMS_KEY_INACCESSIBLE,
        EncryptionStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EncryptionStatus = EncryptionStatus'
  { fromEncryptionStatus ::
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

pattern EncryptionStatus_ENABLED :: EncryptionStatus
pattern EncryptionStatus_ENABLED = EncryptionStatus' "ENABLED"

pattern EncryptionStatus_KMS_KEY_INACCESSIBLE :: EncryptionStatus
pattern EncryptionStatus_KMS_KEY_INACCESSIBLE = EncryptionStatus' "KMS_KEY_INACCESSIBLE"

pattern EncryptionStatus_UPDATING :: EncryptionStatus
pattern EncryptionStatus_UPDATING = EncryptionStatus' "UPDATING"

{-# COMPLETE
  EncryptionStatus_ENABLED,
  EncryptionStatus_KMS_KEY_INACCESSIBLE,
  EncryptionStatus_UPDATING,
  EncryptionStatus'
  #-}
