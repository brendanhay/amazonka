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
        Composite,
        CustomPayload,
        PlainText,
        Ssml
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MessageFormatType = MessageFormatType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Composite :: MessageFormatType
pattern Composite = MessageFormatType' "Composite"

pattern CustomPayload :: MessageFormatType
pattern CustomPayload = MessageFormatType' "CustomPayload"

pattern PlainText :: MessageFormatType
pattern PlainText = MessageFormatType' "PlainText"

pattern Ssml :: MessageFormatType
pattern Ssml = MessageFormatType' "SSML"

{-# COMPLETE
  Composite,
  CustomPayload,
  PlainText,
  Ssml,
  MessageFormatType'
  #-}
