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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.VoiceMessageBodyTextType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.VoiceMessageBodyTextType
  ( VoiceMessageBodyTextType
      ( ..,
        VoiceMessageBodyTextType_SSML,
        VoiceMessageBodyTextType_TEXT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype VoiceMessageBodyTextType = VoiceMessageBodyTextType'
  { fromVoiceMessageBodyTextType ::
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

pattern VoiceMessageBodyTextType_SSML :: VoiceMessageBodyTextType
pattern VoiceMessageBodyTextType_SSML = VoiceMessageBodyTextType' "SSML"

pattern VoiceMessageBodyTextType_TEXT :: VoiceMessageBodyTextType
pattern VoiceMessageBodyTextType_TEXT = VoiceMessageBodyTextType' "TEXT"

{-# COMPLETE
  VoiceMessageBodyTextType_SSML,
  VoiceMessageBodyTextType_TEXT,
  VoiceMessageBodyTextType'
  #-}
