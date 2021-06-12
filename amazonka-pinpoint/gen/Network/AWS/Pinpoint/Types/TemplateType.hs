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
-- Module      : Network.AWS.Pinpoint.Types.TemplateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateType
  ( TemplateType
      ( ..,
        TemplateType_EMAIL,
        TemplateType_PUSH,
        TemplateType_SMS,
        TemplateType_VOICE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TemplateType = TemplateType'
  { fromTemplateType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern TemplateType_EMAIL :: TemplateType
pattern TemplateType_EMAIL = TemplateType' "EMAIL"

pattern TemplateType_PUSH :: TemplateType
pattern TemplateType_PUSH = TemplateType' "PUSH"

pattern TemplateType_SMS :: TemplateType
pattern TemplateType_SMS = TemplateType' "SMS"

pattern TemplateType_VOICE :: TemplateType
pattern TemplateType_VOICE = TemplateType' "VOICE"

{-# COMPLETE
  TemplateType_EMAIL,
  TemplateType_PUSH,
  TemplateType_SMS,
  TemplateType_VOICE,
  TemplateType'
  #-}
