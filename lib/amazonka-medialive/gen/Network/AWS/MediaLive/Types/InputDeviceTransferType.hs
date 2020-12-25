{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceTransferType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceTransferType
  ( InputDeviceTransferType
      ( InputDeviceTransferType',
        InputDeviceTransferTypeOutgoing,
        InputDeviceTransferTypeIncoming,
        fromInputDeviceTransferType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The type of device transfer. INCOMING for an input device that is being transferred to you, OUTGOING for an input device that you are transferring to another AWS account.
newtype InputDeviceTransferType = InputDeviceTransferType'
  { fromInputDeviceTransferType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern InputDeviceTransferTypeOutgoing :: InputDeviceTransferType
pattern InputDeviceTransferTypeOutgoing = InputDeviceTransferType' "OUTGOING"

pattern InputDeviceTransferTypeIncoming :: InputDeviceTransferType
pattern InputDeviceTransferTypeIncoming = InputDeviceTransferType' "INCOMING"

{-# COMPLETE
  InputDeviceTransferTypeOutgoing,
  InputDeviceTransferTypeIncoming,
  InputDeviceTransferType'
  #-}
