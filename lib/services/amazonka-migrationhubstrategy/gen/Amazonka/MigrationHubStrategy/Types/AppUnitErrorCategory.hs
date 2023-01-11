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
-- Module      : Amazonka.MigrationHubStrategy.Types.AppUnitErrorCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AppUnitErrorCategory
  ( AppUnitErrorCategory
      ( ..,
        AppUnitErrorCategory_CONNECTIVITY_ERROR,
        AppUnitErrorCategory_CREDENTIAL_ERROR,
        AppUnitErrorCategory_OTHER_ERROR,
        AppUnitErrorCategory_PERMISSION_ERROR,
        AppUnitErrorCategory_UNSUPPORTED_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AppUnitErrorCategory = AppUnitErrorCategory'
  { fromAppUnitErrorCategory ::
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

pattern AppUnitErrorCategory_CONNECTIVITY_ERROR :: AppUnitErrorCategory
pattern AppUnitErrorCategory_CONNECTIVITY_ERROR = AppUnitErrorCategory' "CONNECTIVITY_ERROR"

pattern AppUnitErrorCategory_CREDENTIAL_ERROR :: AppUnitErrorCategory
pattern AppUnitErrorCategory_CREDENTIAL_ERROR = AppUnitErrorCategory' "CREDENTIAL_ERROR"

pattern AppUnitErrorCategory_OTHER_ERROR :: AppUnitErrorCategory
pattern AppUnitErrorCategory_OTHER_ERROR = AppUnitErrorCategory' "OTHER_ERROR"

pattern AppUnitErrorCategory_PERMISSION_ERROR :: AppUnitErrorCategory
pattern AppUnitErrorCategory_PERMISSION_ERROR = AppUnitErrorCategory' "PERMISSION_ERROR"

pattern AppUnitErrorCategory_UNSUPPORTED_ERROR :: AppUnitErrorCategory
pattern AppUnitErrorCategory_UNSUPPORTED_ERROR = AppUnitErrorCategory' "UNSUPPORTED_ERROR"

{-# COMPLETE
  AppUnitErrorCategory_CONNECTIVITY_ERROR,
  AppUnitErrorCategory_CREDENTIAL_ERROR,
  AppUnitErrorCategory_OTHER_ERROR,
  AppUnitErrorCategory_PERMISSION_ERROR,
  AppUnitErrorCategory_UNSUPPORTED_ERROR,
  AppUnitErrorCategory'
  #-}
