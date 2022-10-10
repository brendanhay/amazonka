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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.SenderIdFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.SenderIdFilterName
  ( SenderIdFilterName
      ( ..,
        SenderIdFilterName_Iso_country_code,
        SenderIdFilterName_Message_type,
        SenderIdFilterName_Sender_id
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SenderIdFilterName = SenderIdFilterName'
  { fromSenderIdFilterName ::
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

pattern SenderIdFilterName_Iso_country_code :: SenderIdFilterName
pattern SenderIdFilterName_Iso_country_code = SenderIdFilterName' "iso-country-code"

pattern SenderIdFilterName_Message_type :: SenderIdFilterName
pattern SenderIdFilterName_Message_type = SenderIdFilterName' "message-type"

pattern SenderIdFilterName_Sender_id :: SenderIdFilterName
pattern SenderIdFilterName_Sender_id = SenderIdFilterName' "sender-id"

{-# COMPLETE
  SenderIdFilterName_Iso_country_code,
  SenderIdFilterName_Message_type,
  SenderIdFilterName_Sender_id,
  SenderIdFilterName'
  #-}
