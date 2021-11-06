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
-- Module      : Amazonka.S3.Types.ObjectLockLegalHoldStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectLockLegalHoldStatus
  ( ObjectLockLegalHoldStatus
      ( ..,
        ObjectLockLegalHoldStatus_OFF,
        ObjectLockLegalHoldStatus_ON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype ObjectLockLegalHoldStatus = ObjectLockLegalHoldStatus'
  { fromObjectLockLegalHoldStatus ::
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

pattern ObjectLockLegalHoldStatus_OFF :: ObjectLockLegalHoldStatus
pattern ObjectLockLegalHoldStatus_OFF = ObjectLockLegalHoldStatus' "OFF"

pattern ObjectLockLegalHoldStatus_ON :: ObjectLockLegalHoldStatus
pattern ObjectLockLegalHoldStatus_ON = ObjectLockLegalHoldStatus' "ON"

{-# COMPLETE
  ObjectLockLegalHoldStatus_OFF,
  ObjectLockLegalHoldStatus_ON,
  ObjectLockLegalHoldStatus'
  #-}
