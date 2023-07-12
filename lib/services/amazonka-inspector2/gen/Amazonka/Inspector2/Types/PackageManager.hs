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
-- Module      : Amazonka.Inspector2.Types.PackageManager
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.PackageManager
  ( PackageManager
      ( ..,
        PackageManager_BUNDLER,
        PackageManager_CARGO,
        PackageManager_COMPOSER,
        PackageManager_GOBINARY,
        PackageManager_GOMOD,
        PackageManager_JAR,
        PackageManager_NODEPKG,
        PackageManager_NPM,
        PackageManager_NUGET,
        PackageManager_OS,
        PackageManager_PIP,
        PackageManager_PIPENV,
        PackageManager_POETRY,
        PackageManager_POM,
        PackageManager_PYTHONPKG,
        PackageManager_YARN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PackageManager = PackageManager'
  { fromPackageManager ::
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

pattern PackageManager_BUNDLER :: PackageManager
pattern PackageManager_BUNDLER = PackageManager' "BUNDLER"

pattern PackageManager_CARGO :: PackageManager
pattern PackageManager_CARGO = PackageManager' "CARGO"

pattern PackageManager_COMPOSER :: PackageManager
pattern PackageManager_COMPOSER = PackageManager' "COMPOSER"

pattern PackageManager_GOBINARY :: PackageManager
pattern PackageManager_GOBINARY = PackageManager' "GOBINARY"

pattern PackageManager_GOMOD :: PackageManager
pattern PackageManager_GOMOD = PackageManager' "GOMOD"

pattern PackageManager_JAR :: PackageManager
pattern PackageManager_JAR = PackageManager' "JAR"

pattern PackageManager_NODEPKG :: PackageManager
pattern PackageManager_NODEPKG = PackageManager' "NODEPKG"

pattern PackageManager_NPM :: PackageManager
pattern PackageManager_NPM = PackageManager' "NPM"

pattern PackageManager_NUGET :: PackageManager
pattern PackageManager_NUGET = PackageManager' "NUGET"

pattern PackageManager_OS :: PackageManager
pattern PackageManager_OS = PackageManager' "OS"

pattern PackageManager_PIP :: PackageManager
pattern PackageManager_PIP = PackageManager' "PIP"

pattern PackageManager_PIPENV :: PackageManager
pattern PackageManager_PIPENV = PackageManager' "PIPENV"

pattern PackageManager_POETRY :: PackageManager
pattern PackageManager_POETRY = PackageManager' "POETRY"

pattern PackageManager_POM :: PackageManager
pattern PackageManager_POM = PackageManager' "POM"

pattern PackageManager_PYTHONPKG :: PackageManager
pattern PackageManager_PYTHONPKG = PackageManager' "PYTHONPKG"

pattern PackageManager_YARN :: PackageManager
pattern PackageManager_YARN = PackageManager' "YARN"

{-# COMPLETE
  PackageManager_BUNDLER,
  PackageManager_CARGO,
  PackageManager_COMPOSER,
  PackageManager_GOBINARY,
  PackageManager_GOMOD,
  PackageManager_JAR,
  PackageManager_NODEPKG,
  PackageManager_NPM,
  PackageManager_NUGET,
  PackageManager_OS,
  PackageManager_PIP,
  PackageManager_PIPENV,
  PackageManager_POETRY,
  PackageManager_POM,
  PackageManager_PYTHONPKG,
  PackageManager_YARN,
  PackageManager'
  #-}
