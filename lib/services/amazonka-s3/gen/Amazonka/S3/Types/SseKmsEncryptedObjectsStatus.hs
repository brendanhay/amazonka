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
-- Module      : Amazonka.S3.Types.SseKmsEncryptedObjectsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.SseKmsEncryptedObjectsStatus
  ( SseKmsEncryptedObjectsStatus
      ( ..,
        SseKmsEncryptedObjectsStatus_Disabled,
        SseKmsEncryptedObjectsStatus_Enabled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype SseKmsEncryptedObjectsStatus = SseKmsEncryptedObjectsStatus'
  { fromSseKmsEncryptedObjectsStatus ::
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

pattern SseKmsEncryptedObjectsStatus_Disabled :: SseKmsEncryptedObjectsStatus
pattern SseKmsEncryptedObjectsStatus_Disabled = SseKmsEncryptedObjectsStatus' "Disabled"

pattern SseKmsEncryptedObjectsStatus_Enabled :: SseKmsEncryptedObjectsStatus
pattern SseKmsEncryptedObjectsStatus_Enabled = SseKmsEncryptedObjectsStatus' "Enabled"

{-# COMPLETE
  SseKmsEncryptedObjectsStatus_Disabled,
  SseKmsEncryptedObjectsStatus_Enabled,
  SseKmsEncryptedObjectsStatus'
  #-}
