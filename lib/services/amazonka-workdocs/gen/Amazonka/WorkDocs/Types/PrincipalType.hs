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
-- Module      : Amazonka.WorkDocs.Types.PrincipalType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.PrincipalType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PrincipalType = PrincipalType'
  { fromPrincipalType ::
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
