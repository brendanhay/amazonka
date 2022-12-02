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
-- Module      : Amazonka.AccessAnalyzer.Types.AclPermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AclPermission
  ( AclPermission
      ( ..,
        AclPermission_FULL_CONTROL,
        AclPermission_READ,
        AclPermission_READ_ACP,
        AclPermission_WRITE,
        AclPermission_WRITE_ACP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AclPermission = AclPermission'
  { fromAclPermission ::
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

pattern AclPermission_FULL_CONTROL :: AclPermission
pattern AclPermission_FULL_CONTROL = AclPermission' "FULL_CONTROL"

pattern AclPermission_READ :: AclPermission
pattern AclPermission_READ = AclPermission' "READ"

pattern AclPermission_READ_ACP :: AclPermission
pattern AclPermission_READ_ACP = AclPermission' "READ_ACP"

pattern AclPermission_WRITE :: AclPermission
pattern AclPermission_WRITE = AclPermission' "WRITE"

pattern AclPermission_WRITE_ACP :: AclPermission
pattern AclPermission_WRITE_ACP = AclPermission' "WRITE_ACP"

{-# COMPLETE
  AclPermission_FULL_CONTROL,
  AclPermission_READ,
  AclPermission_READ_ACP,
  AclPermission_WRITE,
  AclPermission_WRITE_ACP,
  AclPermission'
  #-}
