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
-- Module      : Amazonka.WorkDocs.Types.RoleType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.RoleType
  ( RoleType
      ( ..,
        RoleType_CONTRIBUTOR,
        RoleType_COOWNER,
        RoleType_OWNER,
        RoleType_VIEWER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RoleType = RoleType'
  { fromRoleType ::
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

pattern RoleType_CONTRIBUTOR :: RoleType
pattern RoleType_CONTRIBUTOR = RoleType' "CONTRIBUTOR"

pattern RoleType_COOWNER :: RoleType
pattern RoleType_COOWNER = RoleType' "COOWNER"

pattern RoleType_OWNER :: RoleType
pattern RoleType_OWNER = RoleType' "OWNER"

pattern RoleType_VIEWER :: RoleType
pattern RoleType_VIEWER = RoleType' "VIEWER"

{-# COMPLETE
  RoleType_CONTRIBUTOR,
  RoleType_COOWNER,
  RoleType_OWNER,
  RoleType_VIEWER,
  RoleType'
  #-}
