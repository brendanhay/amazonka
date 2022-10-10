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
-- Module      : Amazonka.ComprehendMedical.Types.ICD10CMRelationshipType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.ICD10CMRelationshipType
  ( ICD10CMRelationshipType
      ( ..,
        ICD10CMRelationshipType_OVERLAP,
        ICD10CMRelationshipType_SYSTEM_ORGAN_SITE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ICD10CMRelationshipType = ICD10CMRelationshipType'
  { fromICD10CMRelationshipType ::
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

pattern ICD10CMRelationshipType_OVERLAP :: ICD10CMRelationshipType
pattern ICD10CMRelationshipType_OVERLAP = ICD10CMRelationshipType' "OVERLAP"

pattern ICD10CMRelationshipType_SYSTEM_ORGAN_SITE :: ICD10CMRelationshipType
pattern ICD10CMRelationshipType_SYSTEM_ORGAN_SITE = ICD10CMRelationshipType' "SYSTEM_ORGAN_SITE"

{-# COMPLETE
  ICD10CMRelationshipType_OVERLAP,
  ICD10CMRelationshipType_SYSTEM_ORGAN_SITE,
  ICD10CMRelationshipType'
  #-}
