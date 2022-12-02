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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.KeywordAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.KeywordAction
  ( KeywordAction
      ( ..,
        KeywordAction_AUTOMATIC_RESPONSE,
        KeywordAction_OPT_IN,
        KeywordAction_OPT_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KeywordAction = KeywordAction'
  { fromKeywordAction ::
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

pattern KeywordAction_AUTOMATIC_RESPONSE :: KeywordAction
pattern KeywordAction_AUTOMATIC_RESPONSE = KeywordAction' "AUTOMATIC_RESPONSE"

pattern KeywordAction_OPT_IN :: KeywordAction
pattern KeywordAction_OPT_IN = KeywordAction' "OPT_IN"

pattern KeywordAction_OPT_OUT :: KeywordAction
pattern KeywordAction_OPT_OUT = KeywordAction' "OPT_OUT"

{-# COMPLETE
  KeywordAction_AUTOMATIC_RESPONSE,
  KeywordAction_OPT_IN,
  KeywordAction_OPT_OUT,
  KeywordAction'
  #-}
