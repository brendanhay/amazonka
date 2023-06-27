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
-- Module      : Amazonka.WorkDocs.Types.PrincipalRoleType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.PrincipalRoleType
  ( PrincipalRoleType
      ( ..,
        PrincipalRoleType_CONTRIBUTOR,
        PrincipalRoleType_COOWNER,
        PrincipalRoleType_OWNER,
        PrincipalRoleType_VIEWER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PrincipalRoleType = PrincipalRoleType'
  { fromPrincipalRoleType ::
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

pattern PrincipalRoleType_CONTRIBUTOR :: PrincipalRoleType
pattern PrincipalRoleType_CONTRIBUTOR = PrincipalRoleType' "CONTRIBUTOR"

pattern PrincipalRoleType_COOWNER :: PrincipalRoleType
pattern PrincipalRoleType_COOWNER = PrincipalRoleType' "COOWNER"

pattern PrincipalRoleType_OWNER :: PrincipalRoleType
pattern PrincipalRoleType_OWNER = PrincipalRoleType' "OWNER"

pattern PrincipalRoleType_VIEWER :: PrincipalRoleType
pattern PrincipalRoleType_VIEWER = PrincipalRoleType' "VIEWER"

{-# COMPLETE
  PrincipalRoleType_CONTRIBUTOR,
  PrincipalRoleType_COOWNER,
  PrincipalRoleType_OWNER,
  PrincipalRoleType_VIEWER,
  PrincipalRoleType'
  #-}
