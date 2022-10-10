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
-- Module      : Amazonka.WorkMail.Types.ImpersonationRoleType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.ImpersonationRoleType
  ( ImpersonationRoleType
      ( ..,
        ImpersonationRoleType_FULL_ACCESS,
        ImpersonationRoleType_READ_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ImpersonationRoleType = ImpersonationRoleType'
  { fromImpersonationRoleType ::
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

pattern ImpersonationRoleType_FULL_ACCESS :: ImpersonationRoleType
pattern ImpersonationRoleType_FULL_ACCESS = ImpersonationRoleType' "FULL_ACCESS"

pattern ImpersonationRoleType_READ_ONLY :: ImpersonationRoleType
pattern ImpersonationRoleType_READ_ONLY = ImpersonationRoleType' "READ_ONLY"

{-# COMPLETE
  ImpersonationRoleType_FULL_ACCESS,
  ImpersonationRoleType_READ_ONLY,
  ImpersonationRoleType'
  #-}
