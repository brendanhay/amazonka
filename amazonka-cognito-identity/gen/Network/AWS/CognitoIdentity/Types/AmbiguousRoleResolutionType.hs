{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype AmbiguousRoleResolutionType = AmbiguousRoleResolutionType'
  { fromAmbiguousRoleResolutionType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
