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
-- Module      : Network.AWS.SSM.Types.AssociationComplianceSeverity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationComplianceSeverity
  ( AssociationComplianceSeverity
      ( ..,
        AssociationComplianceSeverity_CRITICAL,
        AssociationComplianceSeverity_HIGH,
        AssociationComplianceSeverity_LOW,
        AssociationComplianceSeverity_MEDIUM,
        AssociationComplianceSeverity_UNSPECIFIED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AssociationComplianceSeverity = AssociationComplianceSeverity'
  { fromAssociationComplianceSeverity ::
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

pattern AssociationComplianceSeverity_CRITICAL :: AssociationComplianceSeverity
pattern AssociationComplianceSeverity_CRITICAL = AssociationComplianceSeverity' "CRITICAL"

pattern AssociationComplianceSeverity_HIGH :: AssociationComplianceSeverity
pattern AssociationComplianceSeverity_HIGH = AssociationComplianceSeverity' "HIGH"

pattern AssociationComplianceSeverity_LOW :: AssociationComplianceSeverity
pattern AssociationComplianceSeverity_LOW = AssociationComplianceSeverity' "LOW"

pattern AssociationComplianceSeverity_MEDIUM :: AssociationComplianceSeverity
pattern AssociationComplianceSeverity_MEDIUM = AssociationComplianceSeverity' "MEDIUM"

pattern AssociationComplianceSeverity_UNSPECIFIED :: AssociationComplianceSeverity
pattern AssociationComplianceSeverity_UNSPECIFIED = AssociationComplianceSeverity' "UNSPECIFIED"

{-# COMPLETE
  AssociationComplianceSeverity_CRITICAL,
  AssociationComplianceSeverity_HIGH,
  AssociationComplianceSeverity_LOW,
  AssociationComplianceSeverity_MEDIUM,
  AssociationComplianceSeverity_UNSPECIFIED,
  AssociationComplianceSeverity'
  #-}
