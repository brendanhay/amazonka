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
-- Module      : Amazonka.CodeArtifact.Types.PackageVersionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageVersionStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PackageVersionStatus = PackageVersionStatus'
  { fromPackageVersionStatus ::
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
