{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.MessageFormatType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.MessageFormatType
  ( MessageFormatType
      ( MessageFormatType',
        MessageFormatTypePlainText,
        MessageFormatTypeCustomPayload,
        MessageFormatTypeSsml,
        MessageFormatTypeComposite,
        fromMessageFormatType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MessageFormatType = MessageFormatType'
  { fromMessageFormatType ::
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

pattern MessageFormatTypePlainText :: MessageFormatType
pattern MessageFormatTypePlainText = MessageFormatType' "PlainText"

pattern MessageFormatTypeCustomPayload :: MessageFormatType
pattern MessageFormatTypeCustomPayload = MessageFormatType' "CustomPayload"

pattern MessageFormatTypeSsml :: MessageFormatType
pattern MessageFormatTypeSsml = MessageFormatType' "SSML"

pattern MessageFormatTypeComposite :: MessageFormatType
pattern MessageFormatTypeComposite = MessageFormatType' "Composite"

{-# COMPLETE
  MessageFormatTypePlainText,
  MessageFormatTypeCustomPayload,
  MessageFormatTypeSsml,
  MessageFormatTypeComposite,
  MessageFormatType'
  #-}
