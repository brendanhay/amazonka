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
-- Module      : Network.AWS.S3.Types.ObjectLockLegalHoldStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockLegalHoldStatus
  ( ObjectLockLegalHoldStatus
      ( ..,
        ObjectLockLegalHoldStatus_OFF,
        ObjectLockLegalHoldStatus_ON
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.S3.Internal

newtype ObjectLockLegalHoldStatus = ObjectLockLegalHoldStatus'
  { fromObjectLockLegalHoldStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ObjectLockLegalHoldStatus_OFF :: ObjectLockLegalHoldStatus
pattern ObjectLockLegalHoldStatus_OFF = ObjectLockLegalHoldStatus' "OFF"

pattern ObjectLockLegalHoldStatus_ON :: ObjectLockLegalHoldStatus
pattern ObjectLockLegalHoldStatus_ON = ObjectLockLegalHoldStatus' "ON"

{-# COMPLETE
  ObjectLockLegalHoldStatus_OFF,
  ObjectLockLegalHoldStatus_ON,
  ObjectLockLegalHoldStatus'
  #-}
