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
-- Module      : Network.AWS.WorkDocs.Types.RoleType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.RoleType
  ( RoleType
      ( ..,
        RoleType_CONTRIBUTOR,
        RoleType_COOWNER,
        RoleType_OWNER,
        RoleType_VIEWER
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RoleType = RoleType'
  { fromRoleType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
