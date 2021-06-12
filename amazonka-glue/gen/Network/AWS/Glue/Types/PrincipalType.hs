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
-- Module      : Network.AWS.Glue.Types.PrincipalType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PrincipalType
  ( PrincipalType
      ( ..,
        PrincipalType_GROUP,
        PrincipalType_ROLE,
        PrincipalType_USER
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PrincipalType = PrincipalType'
  { fromPrincipalType ::
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

pattern PrincipalType_GROUP :: PrincipalType
pattern PrincipalType_GROUP = PrincipalType' "GROUP"

pattern PrincipalType_ROLE :: PrincipalType
pattern PrincipalType_ROLE = PrincipalType' "ROLE"

pattern PrincipalType_USER :: PrincipalType
pattern PrincipalType_USER = PrincipalType' "USER"

{-# COMPLETE
  PrincipalType_GROUP,
  PrincipalType_ROLE,
  PrincipalType_USER,
  PrincipalType'
  #-}
