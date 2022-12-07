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
-- Module      : Amazonka.VoiceId.Types.ServerSideEncryptionUpdateStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.ServerSideEncryptionUpdateStatus
  ( ServerSideEncryptionUpdateStatus
      ( ..,
        ServerSideEncryptionUpdateStatus_COMPLETED,
        ServerSideEncryptionUpdateStatus_FAILED,
        ServerSideEncryptionUpdateStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServerSideEncryptionUpdateStatus = ServerSideEncryptionUpdateStatus'
  { fromServerSideEncryptionUpdateStatus ::
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

pattern ServerSideEncryptionUpdateStatus_COMPLETED :: ServerSideEncryptionUpdateStatus
pattern ServerSideEncryptionUpdateStatus_COMPLETED = ServerSideEncryptionUpdateStatus' "COMPLETED"

pattern ServerSideEncryptionUpdateStatus_FAILED :: ServerSideEncryptionUpdateStatus
pattern ServerSideEncryptionUpdateStatus_FAILED = ServerSideEncryptionUpdateStatus' "FAILED"

pattern ServerSideEncryptionUpdateStatus_IN_PROGRESS :: ServerSideEncryptionUpdateStatus
pattern ServerSideEncryptionUpdateStatus_IN_PROGRESS = ServerSideEncryptionUpdateStatus' "IN_PROGRESS"

{-# COMPLETE
  ServerSideEncryptionUpdateStatus_COMPLETED,
  ServerSideEncryptionUpdateStatus_FAILED,
  ServerSideEncryptionUpdateStatus_IN_PROGRESS,
  ServerSideEncryptionUpdateStatus'
  #-}
