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
        Arch,
        AdvisoryId,
        BugzillaId,
        PatchSet,
        Product,
        ProductFamily,
        Classification,
        CveId,
        Epoch,
        MsrcSeverity,
        Name,
        PatchId,
        Section,
        Priority,
        Repository,
        Release,
        Severity,
        Security,
        Version
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PatchFilterKey = PatchFilterKey' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Arch :: PatchFilterKey
pattern Arch = PatchFilterKey' "ARCH"

pattern AdvisoryId :: PatchFilterKey
pattern AdvisoryId = PatchFilterKey' "ADVISORY_ID"

pattern BugzillaId :: PatchFilterKey
pattern BugzillaId = PatchFilterKey' "BUGZILLA_ID"

pattern PatchSet :: PatchFilterKey
pattern PatchSet = PatchFilterKey' "PATCH_SET"

pattern Product :: PatchFilterKey
pattern Product = PatchFilterKey' "PRODUCT"

pattern ProductFamily :: PatchFilterKey
pattern ProductFamily = PatchFilterKey' "PRODUCT_FAMILY"

pattern Classification :: PatchFilterKey
pattern Classification = PatchFilterKey' "CLASSIFICATION"

pattern CveId :: PatchFilterKey
pattern CveId = PatchFilterKey' "CVE_ID"

pattern Epoch :: PatchFilterKey
pattern Epoch = PatchFilterKey' "EPOCH"

pattern MsrcSeverity :: PatchFilterKey
pattern MsrcSeverity = PatchFilterKey' "MSRC_SEVERITY"

pattern Name :: PatchFilterKey
pattern Name = PatchFilterKey' "NAME"

pattern PatchId :: PatchFilterKey
pattern PatchId = PatchFilterKey' "PATCH_ID"

pattern Section :: PatchFilterKey
pattern Section = PatchFilterKey' "SECTION"

pattern Priority :: PatchFilterKey
pattern Priority = PatchFilterKey' "PRIORITY"

pattern Repository :: PatchFilterKey
pattern Repository = PatchFilterKey' "REPOSITORY"

pattern Release :: PatchFilterKey
pattern Release = PatchFilterKey' "RELEASE"

pattern Severity :: PatchFilterKey
pattern Severity = PatchFilterKey' "SEVERITY"

pattern Security :: PatchFilterKey
pattern Security = PatchFilterKey' "SECURITY"

pattern Version :: PatchFilterKey
pattern Version = PatchFilterKey' "VERSION"

{-# COMPLETE
  Arch,
  AdvisoryId,
  BugzillaId,
  PatchSet,
  Product,
  ProductFamily,
  Classification,
  CveId,
  Epoch,
  MsrcSeverity,
  Name,
  PatchId,
  Section,
  Priority,
  Repository,
  Release,
  Severity,
  Security,
  Version,
  PatchFilterKey'
  #-}
