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
-- Module      : Amazonka.EC2.Types.PrincipalType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PrincipalType
  ( PrincipalType
      ( ..,
        PrincipalType_Account,
        PrincipalType_All,
        PrincipalType_OrganizationUnit,
        PrincipalType_Role,
        PrincipalType_Service,
        PrincipalType_User
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype PrincipalType = PrincipalType'
  { fromPrincipalType ::
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

pattern PrincipalType_Account :: PrincipalType
pattern PrincipalType_Account = PrincipalType' "Account"

pattern PrincipalType_All :: PrincipalType
pattern PrincipalType_All = PrincipalType' "All"

pattern PrincipalType_OrganizationUnit :: PrincipalType
pattern PrincipalType_OrganizationUnit = PrincipalType' "OrganizationUnit"

pattern PrincipalType_Role :: PrincipalType
pattern PrincipalType_Role = PrincipalType' "Role"

pattern PrincipalType_Service :: PrincipalType
pattern PrincipalType_Service = PrincipalType' "Service"

pattern PrincipalType_User :: PrincipalType
pattern PrincipalType_User = PrincipalType' "User"

{-# COMPLETE
  PrincipalType_Account,
  PrincipalType_All,
  PrincipalType_OrganizationUnit,
  PrincipalType_Role,
  PrincipalType_Service,
  PrincipalType_User,
  PrincipalType'
  #-}
