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
-- Module      : Amazonka.Proton.Types.TemplateVersionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.TemplateVersionStatus
  ( TemplateVersionStatus
      ( ..,
        TemplateVersionStatus_DRAFT,
        TemplateVersionStatus_PUBLISHED,
        TemplateVersionStatus_REGISTRATION_FAILED,
        TemplateVersionStatus_REGISTRATION_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TemplateVersionStatus = TemplateVersionStatus'
  { fromTemplateVersionStatus ::
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
