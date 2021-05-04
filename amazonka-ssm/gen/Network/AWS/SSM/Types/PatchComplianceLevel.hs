{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchComplianceLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchComplianceLevel
  ( PatchComplianceLevel
      ( ..,
        PatchComplianceLevel_CRITICAL,
        PatchComplianceLevel_HIGH,
        PatchComplianceLevel_INFORMATIONAL,
        PatchComplianceLevel_LOW,
        PatchComplianceLevel_MEDIUM,
        PatchComplianceLevel_UNSPECIFIED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype PatchComplianceLevel = PatchComplianceLevel'
  { fromPatchComplianceLevel ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern PatchComplianceLevel_CRITICAL :: PatchComplianceLevel
pattern PatchComplianceLevel_CRITICAL = PatchComplianceLevel' "CRITICAL"

pattern PatchComplianceLevel_HIGH :: PatchComplianceLevel
pattern PatchComplianceLevel_HIGH = PatchComplianceLevel' "HIGH"

pattern PatchComplianceLevel_INFORMATIONAL :: PatchComplianceLevel
pattern PatchComplianceLevel_INFORMATIONAL = PatchComplianceLevel' "INFORMATIONAL"

pattern PatchComplianceLevel_LOW :: PatchComplianceLevel
pattern PatchComplianceLevel_LOW = PatchComplianceLevel' "LOW"

pattern PatchComplianceLevel_MEDIUM :: PatchComplianceLevel
pattern PatchComplianceLevel_MEDIUM = PatchComplianceLevel' "MEDIUM"

pattern PatchComplianceLevel_UNSPECIFIED :: PatchComplianceLevel
pattern PatchComplianceLevel_UNSPECIFIED = PatchComplianceLevel' "UNSPECIFIED"

{-# COMPLETE
  PatchComplianceLevel_CRITICAL,
  PatchComplianceLevel_HIGH,
  PatchComplianceLevel_INFORMATIONAL,
  PatchComplianceLevel_LOW,
  PatchComplianceLevel_MEDIUM,
  PatchComplianceLevel_UNSPECIFIED,
  PatchComplianceLevel'
  #-}
