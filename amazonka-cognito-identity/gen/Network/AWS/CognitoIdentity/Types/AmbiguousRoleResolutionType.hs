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
-- Module      : Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType
  ( AmbiguousRoleResolutionType
      ( ..,
        AmbiguousRoleResolutionType_AuthenticatedRole,
        AmbiguousRoleResolutionType_Deny
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AmbiguousRoleResolutionType = AmbiguousRoleResolutionType'
  { fromAmbiguousRoleResolutionType ::
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

pattern AmbiguousRoleResolutionType_AuthenticatedRole :: AmbiguousRoleResolutionType
pattern AmbiguousRoleResolutionType_AuthenticatedRole = AmbiguousRoleResolutionType' "AuthenticatedRole"

pattern AmbiguousRoleResolutionType_Deny :: AmbiguousRoleResolutionType
pattern AmbiguousRoleResolutionType_Deny = AmbiguousRoleResolutionType' "Deny"

{-# COMPLETE
  AmbiguousRoleResolutionType_AuthenticatedRole,
  AmbiguousRoleResolutionType_Deny,
  AmbiguousRoleResolutionType'
  #-}
