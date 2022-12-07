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
-- Module      : Amazonka.LexRuntime.Types.MessageFormatType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.MessageFormatType
  ( MessageFormatType
      ( ..,
        MessageFormatType_Composite,
        MessageFormatType_CustomPayload,
        MessageFormatType_PlainText,
        MessageFormatType_SSML
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MessageFormatType = MessageFormatType'
  { fromMessageFormatType ::
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
