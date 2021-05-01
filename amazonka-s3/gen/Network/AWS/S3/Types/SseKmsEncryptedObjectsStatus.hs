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
-- Module      : Network.AWS.S3.Types.SseKmsEncryptedObjectsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SseKmsEncryptedObjectsStatus
  ( SseKmsEncryptedObjectsStatus
      ( ..,
        SseKmsEncryptedObjectsStatus_Disabled,
        SseKmsEncryptedObjectsStatus_Enabled
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

newtype SseKmsEncryptedObjectsStatus = SseKmsEncryptedObjectsStatus'
  { fromSseKmsEncryptedObjectsStatus ::
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

pattern SseKmsEncryptedObjectsStatus_Disabled :: SseKmsEncryptedObjectsStatus
pattern SseKmsEncryptedObjectsStatus_Disabled = SseKmsEncryptedObjectsStatus' "Disabled"

pattern SseKmsEncryptedObjectsStatus_Enabled :: SseKmsEncryptedObjectsStatus
pattern SseKmsEncryptedObjectsStatus_Enabled = SseKmsEncryptedObjectsStatus' "Enabled"

{-# COMPLETE
  SseKmsEncryptedObjectsStatus_Disabled,
  SseKmsEncryptedObjectsStatus_Enabled,
  SseKmsEncryptedObjectsStatus'
  #-}
