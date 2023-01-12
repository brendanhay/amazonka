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
-- Module      : Amazonka.Pinpoint.Types.TemplateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.TemplateType
  ( TemplateType
      ( ..,
        TemplateType_EMAIL,
        TemplateType_INAPP,
        TemplateType_PUSH,
        TemplateType_SMS,
        TemplateType_VOICE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TemplateType = TemplateType'
  { fromTemplateType ::
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

pattern TemplateType_EMAIL :: TemplateType
pattern TemplateType_EMAIL = TemplateType' "EMAIL"

pattern TemplateType_INAPP :: TemplateType
pattern TemplateType_INAPP = TemplateType' "INAPP"

pattern TemplateType_PUSH :: TemplateType
pattern TemplateType_PUSH = TemplateType' "PUSH"

pattern TemplateType_SMS :: TemplateType
pattern TemplateType_SMS = TemplateType' "SMS"

pattern TemplateType_VOICE :: TemplateType
pattern TemplateType_VOICE = TemplateType' "VOICE"

{-# COMPLETE
  TemplateType_EMAIL,
  TemplateType_INAPP,
  TemplateType_PUSH,
  TemplateType_SMS,
  TemplateType_VOICE,
  TemplateType'
  #-}
