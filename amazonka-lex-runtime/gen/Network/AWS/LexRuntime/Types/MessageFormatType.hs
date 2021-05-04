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
-- Module      : Network.AWS.LexRuntime.Types.MessageFormatType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.MessageFormatType
  ( MessageFormatType
      ( ..,
        MessageFormatType_Composite,
        MessageFormatType_CustomPayload,
        MessageFormatType_PlainText,
        MessageFormatType_SSML
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype MessageFormatType = MessageFormatType'
  { fromMessageFormatType ::
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

pattern MessageFormatType_Composite :: MessageFormatType
pattern MessageFormatType_Composite = MessageFormatType' "Composite"

pattern MessageFormatType_CustomPayload :: MessageFormatType
pattern MessageFormatType_CustomPayload = MessageFormatType' "CustomPayload"

pattern MessageFormatType_PlainText :: MessageFormatType
pattern MessageFormatType_PlainText = MessageFormatType' "PlainText"

pattern MessageFormatType_SSML :: MessageFormatType
pattern MessageFormatType_SSML = MessageFormatType' "SSML"

{-# COMPLETE
  MessageFormatType_Composite,
  MessageFormatType_CustomPayload,
  MessageFormatType_PlainText,
  MessageFormatType_SSML,
  MessageFormatType'
  #-}
