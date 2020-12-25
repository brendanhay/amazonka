{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchFilterKey
  ( PatchFilterKey
      ( PatchFilterKey',
        PatchFilterKeyArch,
        PatchFilterKeyAdvisoryId,
        PatchFilterKeyBugzillaId,
        PatchFilterKeyPatchSet,
        PatchFilterKeyProduct,
        PatchFilterKeyProductFamily,
        PatchFilterKeyClassification,
        PatchFilterKeyCveId,
        PatchFilterKeyEpoch,
        PatchFilterKeyMsrcSeverity,
        PatchFilterKeyName,
        PatchFilterKeyPatchId,
        PatchFilterKeySection,
        PatchFilterKeyPriority,
        PatchFilterKeyRepository,
        PatchFilterKeyRelease,
        PatchFilterKeySeverity,
        PatchFilterKeySecurity,
        PatchFilterKeyVersion,
        fromPatchFilterKey
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PatchFilterKey = PatchFilterKey'
  { fromPatchFilterKey ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern PatchFilterKeyArch :: PatchFilterKey
pattern PatchFilterKeyArch = PatchFilterKey' "ARCH"

pattern PatchFilterKeyAdvisoryId :: PatchFilterKey
pattern PatchFilterKeyAdvisoryId = PatchFilterKey' "ADVISORY_ID"

pattern PatchFilterKeyBugzillaId :: PatchFilterKey
pattern PatchFilterKeyBugzillaId = PatchFilterKey' "BUGZILLA_ID"

pattern PatchFilterKeyPatchSet :: PatchFilterKey
pattern PatchFilterKeyPatchSet = PatchFilterKey' "PATCH_SET"

pattern PatchFilterKeyProduct :: PatchFilterKey
pattern PatchFilterKeyProduct = PatchFilterKey' "PRODUCT"

pattern PatchFilterKeyProductFamily :: PatchFilterKey
pattern PatchFilterKeyProductFamily = PatchFilterKey' "PRODUCT_FAMILY"

pattern PatchFilterKeyClassification :: PatchFilterKey
pattern PatchFilterKeyClassification = PatchFilterKey' "CLASSIFICATION"

pattern PatchFilterKeyCveId :: PatchFilterKey
pattern PatchFilterKeyCveId = PatchFilterKey' "CVE_ID"

pattern PatchFilterKeyEpoch :: PatchFilterKey
pattern PatchFilterKeyEpoch = PatchFilterKey' "EPOCH"

pattern PatchFilterKeyMsrcSeverity :: PatchFilterKey
pattern PatchFilterKeyMsrcSeverity = PatchFilterKey' "MSRC_SEVERITY"

pattern PatchFilterKeyName :: PatchFilterKey
pattern PatchFilterKeyName = PatchFilterKey' "NAME"

pattern PatchFilterKeyPatchId :: PatchFilterKey
pattern PatchFilterKeyPatchId = PatchFilterKey' "PATCH_ID"

pattern PatchFilterKeySection :: PatchFilterKey
pattern PatchFilterKeySection = PatchFilterKey' "SECTION"

pattern PatchFilterKeyPriority :: PatchFilterKey
pattern PatchFilterKeyPriority = PatchFilterKey' "PRIORITY"

pattern PatchFilterKeyRepository :: PatchFilterKey
pattern PatchFilterKeyRepository = PatchFilterKey' "REPOSITORY"

pattern PatchFilterKeyRelease :: PatchFilterKey
pattern PatchFilterKeyRelease = PatchFilterKey' "RELEASE"

pattern PatchFilterKeySeverity :: PatchFilterKey
pattern PatchFilterKeySeverity = PatchFilterKey' "SEVERITY"

pattern PatchFilterKeySecurity :: PatchFilterKey
pattern PatchFilterKeySecurity = PatchFilterKey' "SECURITY"

pattern PatchFilterKeyVersion :: PatchFilterKey
pattern PatchFilterKeyVersion = PatchFilterKey' "VERSION"

{-# COMPLETE
  PatchFilterKeyArch,
  PatchFilterKeyAdvisoryId,
  PatchFilterKeyBugzillaId,
  PatchFilterKeyPatchSet,
  PatchFilterKeyProduct,
  PatchFilterKeyProductFamily,
  PatchFilterKeyClassification,
  PatchFilterKeyCveId,
  PatchFilterKeyEpoch,
  PatchFilterKeyMsrcSeverity,
  PatchFilterKeyName,
  PatchFilterKeyPatchId,
  PatchFilterKeySection,
  PatchFilterKeyPriority,
  PatchFilterKeyRepository,
  PatchFilterKeyRelease,
  PatchFilterKeySeverity,
  PatchFilterKeySecurity,
  PatchFilterKeyVersion,
  PatchFilterKey'
  #-}
