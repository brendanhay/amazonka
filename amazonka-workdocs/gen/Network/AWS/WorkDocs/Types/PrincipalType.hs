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
-- Module      : Network.AWS.WorkDocs.Types.PrincipalType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.PrincipalType
  ( PrincipalType
      ( ..,
        PrincipalType_ANONYMOUS,
        PrincipalType_GROUP,
        PrincipalType_INVITE,
        PrincipalType_ORGANIZATION,
        PrincipalType_USER
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype PrincipalType = PrincipalType'
  { fromPrincipalType ::
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

pattern PrincipalType_ANONYMOUS :: PrincipalType
pattern PrincipalType_ANONYMOUS = PrincipalType' "ANONYMOUS"

pattern PrincipalType_GROUP :: PrincipalType
pattern PrincipalType_GROUP = PrincipalType' "GROUP"

pattern PrincipalType_INVITE :: PrincipalType
pattern PrincipalType_INVITE = PrincipalType' "INVITE"

pattern PrincipalType_ORGANIZATION :: PrincipalType
pattern PrincipalType_ORGANIZATION = PrincipalType' "ORGANIZATION"

pattern PrincipalType_USER :: PrincipalType
pattern PrincipalType_USER = PrincipalType' "USER"

{-# COMPLETE
  PrincipalType_ANONYMOUS,
  PrincipalType_GROUP,
  PrincipalType_INVITE,
  PrincipalType_ORGANIZATION,
  PrincipalType_USER,
  PrincipalType'
  #-}
