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
-- Module      : Network.AWS.CodeArtifact.Types.PackageVersionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeArtifact.Types.PackageVersionStatus
  ( PackageVersionStatus
      ( ..,
        PackageVersionStatus_Archived,
        PackageVersionStatus_Deleted,
        PackageVersionStatus_Disposed,
        PackageVersionStatus_Published,
        PackageVersionStatus_Unfinished,
        PackageVersionStatus_Unlisted
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PackageVersionStatus = PackageVersionStatus'
  { fromPackageVersionStatus ::
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

pattern PackageVersionStatus_Archived :: PackageVersionStatus
pattern PackageVersionStatus_Archived = PackageVersionStatus' "Archived"

pattern PackageVersionStatus_Deleted :: PackageVersionStatus
pattern PackageVersionStatus_Deleted = PackageVersionStatus' "Deleted"

pattern PackageVersionStatus_Disposed :: PackageVersionStatus
pattern PackageVersionStatus_Disposed = PackageVersionStatus' "Disposed"

pattern PackageVersionStatus_Published :: PackageVersionStatus
pattern PackageVersionStatus_Published = PackageVersionStatus' "Published"

pattern PackageVersionStatus_Unfinished :: PackageVersionStatus
pattern PackageVersionStatus_Unfinished = PackageVersionStatus' "Unfinished"

pattern PackageVersionStatus_Unlisted :: PackageVersionStatus
pattern PackageVersionStatus_Unlisted = PackageVersionStatus' "Unlisted"

{-# COMPLETE
  PackageVersionStatus_Archived,
  PackageVersionStatus_Deleted,
  PackageVersionStatus_Disposed,
  PackageVersionStatus_Published,
  PackageVersionStatus_Unfinished,
  PackageVersionStatus_Unlisted,
  PackageVersionStatus'
  #-}
