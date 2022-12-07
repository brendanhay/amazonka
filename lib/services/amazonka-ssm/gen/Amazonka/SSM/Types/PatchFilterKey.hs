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
-- Module      : Amazonka.SSM.Types.PatchFilterKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.PatchFilterKey
  ( PatchFilterKey
      ( ..,
        PatchFilterKey_ADVISORY_ID,
        PatchFilterKey_ARCH,
        PatchFilterKey_BUGZILLA_ID,
        PatchFilterKey_CLASSIFICATION,
        PatchFilterKey_CVE_ID,
        PatchFilterKey_EPOCH,
        PatchFilterKey_MSRC_SEVERITY,
        PatchFilterKey_NAME,
        PatchFilterKey_PATCH_ID,
        PatchFilterKey_PATCH_SET,
        PatchFilterKey_PRIORITY,
        PatchFilterKey_PRODUCT,
        PatchFilterKey_PRODUCT_FAMILY,
        PatchFilterKey_RELEASE,
        PatchFilterKey_REPOSITORY,
        PatchFilterKey_SECTION,
        PatchFilterKey_SECURITY,
        PatchFilterKey_SEVERITY,
        PatchFilterKey_VERSION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PatchFilterKey = PatchFilterKey'
  { fromPatchFilterKey ::
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

pattern PatchFilterKey_ADVISORY_ID :: PatchFilterKey
pattern PatchFilterKey_ADVISORY_ID = PatchFilterKey' "ADVISORY_ID"

pattern PatchFilterKey_ARCH :: PatchFilterKey
pattern PatchFilterKey_ARCH = PatchFilterKey' "ARCH"

pattern PatchFilterKey_BUGZILLA_ID :: PatchFilterKey
pattern PatchFilterKey_BUGZILLA_ID = PatchFilterKey' "BUGZILLA_ID"

pattern PatchFilterKey_CLASSIFICATION :: PatchFilterKey
pattern PatchFilterKey_CLASSIFICATION = PatchFilterKey' "CLASSIFICATION"

pattern PatchFilterKey_CVE_ID :: PatchFilterKey
pattern PatchFilterKey_CVE_ID = PatchFilterKey' "CVE_ID"

pattern PatchFilterKey_EPOCH :: PatchFilterKey
pattern PatchFilterKey_EPOCH = PatchFilterKey' "EPOCH"

pattern PatchFilterKey_MSRC_SEVERITY :: PatchFilterKey
pattern PatchFilterKey_MSRC_SEVERITY = PatchFilterKey' "MSRC_SEVERITY"

pattern PatchFilterKey_NAME :: PatchFilterKey
pattern PatchFilterKey_NAME = PatchFilterKey' "NAME"

pattern PatchFilterKey_PATCH_ID :: PatchFilterKey
pattern PatchFilterKey_PATCH_ID = PatchFilterKey' "PATCH_ID"

pattern PatchFilterKey_PATCH_SET :: PatchFilterKey
pattern PatchFilterKey_PATCH_SET = PatchFilterKey' "PATCH_SET"

pattern PatchFilterKey_PRIORITY :: PatchFilterKey
pattern PatchFilterKey_PRIORITY = PatchFilterKey' "PRIORITY"

pattern PatchFilterKey_PRODUCT :: PatchFilterKey
pattern PatchFilterKey_PRODUCT = PatchFilterKey' "PRODUCT"

pattern PatchFilterKey_PRODUCT_FAMILY :: PatchFilterKey
pattern PatchFilterKey_PRODUCT_FAMILY = PatchFilterKey' "PRODUCT_FAMILY"

pattern PatchFilterKey_RELEASE :: PatchFilterKey
pattern PatchFilterKey_RELEASE = PatchFilterKey' "RELEASE"

pattern PatchFilterKey_REPOSITORY :: PatchFilterKey
pattern PatchFilterKey_REPOSITORY = PatchFilterKey' "REPOSITORY"

pattern PatchFilterKey_SECTION :: PatchFilterKey
pattern PatchFilterKey_SECTION = PatchFilterKey' "SECTION"

pattern PatchFilterKey_SECURITY :: PatchFilterKey
pattern PatchFilterKey_SECURITY = PatchFilterKey' "SECURITY"

pattern PatchFilterKey_SEVERITY :: PatchFilterKey
pattern PatchFilterKey_SEVERITY = PatchFilterKey' "SEVERITY"

pattern PatchFilterKey_VERSION :: PatchFilterKey
pattern PatchFilterKey_VERSION = PatchFilterKey' "VERSION"

{-# COMPLETE
  PatchFilterKey_ADVISORY_ID,
  PatchFilterKey_ARCH,
  PatchFilterKey_BUGZILLA_ID,
  PatchFilterKey_CLASSIFICATION,
  PatchFilterKey_CVE_ID,
  PatchFilterKey_EPOCH,
  PatchFilterKey_MSRC_SEVERITY,
  PatchFilterKey_NAME,
  PatchFilterKey_PATCH_ID,
  PatchFilterKey_PATCH_SET,
  PatchFilterKey_PRIORITY,
  PatchFilterKey_PRODUCT,
  PatchFilterKey_PRODUCT_FAMILY,
  PatchFilterKey_RELEASE,
  PatchFilterKey_REPOSITORY,
  PatchFilterKey_SECTION,
  PatchFilterKey_SECURITY,
  PatchFilterKey_SEVERITY,
  PatchFilterKey_VERSION,
  PatchFilterKey'
  #-}
