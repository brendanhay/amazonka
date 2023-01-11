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
-- Module      : Amazonka.CodeArtifact.Types.PackageFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageFormat
  ( PackageFormat
      ( ..,
        PackageFormat_Maven,
        PackageFormat_Npm,
        PackageFormat_Nuget,
        PackageFormat_Pypi
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PackageFormat = PackageFormat'
  { fromPackageFormat ::
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

pattern PackageFormat_Maven :: PackageFormat
pattern PackageFormat_Maven = PackageFormat' "maven"

pattern PackageFormat_Npm :: PackageFormat
pattern PackageFormat_Npm = PackageFormat' "npm"

pattern PackageFormat_Nuget :: PackageFormat
pattern PackageFormat_Nuget = PackageFormat' "nuget"

pattern PackageFormat_Pypi :: PackageFormat
pattern PackageFormat_Pypi = PackageFormat' "pypi"

{-# COMPLETE
  PackageFormat_Maven,
  PackageFormat_Npm,
  PackageFormat_Nuget,
  PackageFormat_Pypi,
  PackageFormat'
  #-}
