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
-- Module      : Network.AWS.Proton.Types.TemplateVersionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Proton.Types.TemplateVersionStatus
  ( TemplateVersionStatus
      ( ..,
        TemplateVersionStatus_DRAFT,
        TemplateVersionStatus_PUBLISHED,
        TemplateVersionStatus_REGISTRATION_FAILED,
        TemplateVersionStatus_REGISTRATION_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TemplateVersionStatus = TemplateVersionStatus'
  { fromTemplateVersionStatus ::
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

pattern TemplateVersionStatus_DRAFT :: TemplateVersionStatus
pattern TemplateVersionStatus_DRAFT = TemplateVersionStatus' "DRAFT"

pattern TemplateVersionStatus_PUBLISHED :: TemplateVersionStatus
pattern TemplateVersionStatus_PUBLISHED = TemplateVersionStatus' "PUBLISHED"

pattern TemplateVersionStatus_REGISTRATION_FAILED :: TemplateVersionStatus
pattern TemplateVersionStatus_REGISTRATION_FAILED = TemplateVersionStatus' "REGISTRATION_FAILED"

pattern TemplateVersionStatus_REGISTRATION_IN_PROGRESS :: TemplateVersionStatus
pattern TemplateVersionStatus_REGISTRATION_IN_PROGRESS = TemplateVersionStatus' "REGISTRATION_IN_PROGRESS"

{-# COMPLETE
  TemplateVersionStatus_DRAFT,
  TemplateVersionStatus_PUBLISHED,
  TemplateVersionStatus_REGISTRATION_FAILED,
  TemplateVersionStatus_REGISTRATION_IN_PROGRESS,
  TemplateVersionStatus'
  #-}
