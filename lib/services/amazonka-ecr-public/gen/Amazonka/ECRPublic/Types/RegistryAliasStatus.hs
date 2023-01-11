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
-- Module      : Amazonka.ECRPublic.Types.RegistryAliasStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.RegistryAliasStatus
  ( RegistryAliasStatus
      ( ..,
        RegistryAliasStatus_ACTIVE,
        RegistryAliasStatus_PENDING,
        RegistryAliasStatus_REJECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RegistryAliasStatus = RegistryAliasStatus'
  { fromRegistryAliasStatus ::
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

pattern RegistryAliasStatus_ACTIVE :: RegistryAliasStatus
pattern RegistryAliasStatus_ACTIVE = RegistryAliasStatus' "ACTIVE"

pattern RegistryAliasStatus_PENDING :: RegistryAliasStatus
pattern RegistryAliasStatus_PENDING = RegistryAliasStatus' "PENDING"

pattern RegistryAliasStatus_REJECTED :: RegistryAliasStatus
pattern RegistryAliasStatus_REJECTED = RegistryAliasStatus' "REJECTED"

{-# COMPLETE
  RegistryAliasStatus_ACTIVE,
  RegistryAliasStatus_PENDING,
  RegistryAliasStatus_REJECTED,
  RegistryAliasStatus'
  #-}
