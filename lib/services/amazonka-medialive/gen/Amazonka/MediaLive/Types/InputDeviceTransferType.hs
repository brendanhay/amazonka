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
-- Module      : Amazonka.MediaLive.Types.InputDeviceTransferType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDeviceTransferType
  ( InputDeviceTransferType
      ( ..,
        InputDeviceTransferType_INCOMING,
        InputDeviceTransferType_OUTGOING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of device transfer. INCOMING for an input device that is being
-- transferred to you, OUTGOING for an input device that you are
-- transferring to another AWS account.
newtype InputDeviceTransferType = InputDeviceTransferType'
  { fromInputDeviceTransferType ::
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

pattern InputDeviceTransferType_INCOMING :: InputDeviceTransferType
pattern InputDeviceTransferType_INCOMING = InputDeviceTransferType' "INCOMING"

pattern InputDeviceTransferType_OUTGOING :: InputDeviceTransferType
pattern InputDeviceTransferType_OUTGOING = InputDeviceTransferType' "OUTGOING"

{-# COMPLETE
  InputDeviceTransferType_INCOMING,
  InputDeviceTransferType_OUTGOING,
  InputDeviceTransferType'
  #-}
