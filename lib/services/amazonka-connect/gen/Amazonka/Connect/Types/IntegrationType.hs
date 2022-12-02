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
-- Module      : Amazonka.Connect.Types.IntegrationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.IntegrationType
  ( IntegrationType
      ( ..,
        IntegrationType_CASES_DOMAIN,
        IntegrationType_EVENT,
        IntegrationType_PINPOINT_APP,
        IntegrationType_VOICE_ID,
        IntegrationType_WISDOM_ASSISTANT,
        IntegrationType_WISDOM_KNOWLEDGE_BASE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IntegrationType = IntegrationType'
  { fromIntegrationType ::
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

pattern IntegrationType_CASES_DOMAIN :: IntegrationType
pattern IntegrationType_CASES_DOMAIN = IntegrationType' "CASES_DOMAIN"

pattern IntegrationType_EVENT :: IntegrationType
pattern IntegrationType_EVENT = IntegrationType' "EVENT"

pattern IntegrationType_PINPOINT_APP :: IntegrationType
pattern IntegrationType_PINPOINT_APP = IntegrationType' "PINPOINT_APP"

pattern IntegrationType_VOICE_ID :: IntegrationType
pattern IntegrationType_VOICE_ID = IntegrationType' "VOICE_ID"

pattern IntegrationType_WISDOM_ASSISTANT :: IntegrationType
pattern IntegrationType_WISDOM_ASSISTANT = IntegrationType' "WISDOM_ASSISTANT"

pattern IntegrationType_WISDOM_KNOWLEDGE_BASE :: IntegrationType
pattern IntegrationType_WISDOM_KNOWLEDGE_BASE = IntegrationType' "WISDOM_KNOWLEDGE_BASE"

{-# COMPLETE
  IntegrationType_CASES_DOMAIN,
  IntegrationType_EVENT,
  IntegrationType_PINPOINT_APP,
  IntegrationType_VOICE_ID,
  IntegrationType_WISDOM_ASSISTANT,
  IntegrationType_WISDOM_KNOWLEDGE_BASE,
  IntegrationType'
  #-}
