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
-- Module      : Amazonka.LexV2Models.Types.ImportResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ImportResourceType
  ( ImportResourceType
      ( ..,
        ImportResourceType_Bot,
        ImportResourceType_BotLocale,
        ImportResourceType_CustomVocabulary
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ImportResourceType = ImportResourceType'
  { fromImportResourceType ::
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

pattern ImportResourceType_Bot :: ImportResourceType
pattern ImportResourceType_Bot = ImportResourceType' "Bot"

pattern ImportResourceType_BotLocale :: ImportResourceType
pattern ImportResourceType_BotLocale = ImportResourceType' "BotLocale"

pattern ImportResourceType_CustomVocabulary :: ImportResourceType
pattern ImportResourceType_CustomVocabulary = ImportResourceType' "CustomVocabulary"

{-# COMPLETE
  ImportResourceType_Bot,
  ImportResourceType_BotLocale,
  ImportResourceType_CustomVocabulary,
  ImportResourceType'
  #-}
