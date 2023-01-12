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
-- Module      : Amazonka.QLDB.Types.EncryptionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.EncryptionStatus
  ( EncryptionStatus
      ( ..,
        EncryptionStatus_ENABLED,
        EncryptionStatus_KMS_KEY_INACCESSIBLE,
        EncryptionStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EncryptionStatus = EncryptionStatus'
  { fromEncryptionStatus ::
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
