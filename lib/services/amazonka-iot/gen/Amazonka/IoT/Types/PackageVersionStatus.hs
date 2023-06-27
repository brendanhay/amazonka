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
-- Module      : Amazonka.IoT.Types.PackageVersionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.PackageVersionStatus
  ( PackageVersionStatus
      ( ..,
        PackageVersionStatus_DEPRECATED,
        PackageVersionStatus_DRAFT,
        PackageVersionStatus_PUBLISHED
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

pattern PackageVersionStatus_DEPRECATED :: PackageVersionStatus
pattern PackageVersionStatus_DEPRECATED = PackageVersionStatus' "DEPRECATED"

pattern PackageVersionStatus_DRAFT :: PackageVersionStatus
pattern PackageVersionStatus_DRAFT = PackageVersionStatus' "DRAFT"

pattern PackageVersionStatus_PUBLISHED :: PackageVersionStatus
pattern PackageVersionStatus_PUBLISHED = PackageVersionStatus' "PUBLISHED"

{-# COMPLETE
  PackageVersionStatus_DEPRECATED,
  PackageVersionStatus_DRAFT,
  PackageVersionStatus_PUBLISHED,
  PackageVersionStatus'
  #-}
