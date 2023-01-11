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
-- Module      : Amazonka.WorkMail.Types.PermissionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.PermissionType
  ( PermissionType
      ( ..,
        PermissionType_FULL_ACCESS,
        PermissionType_SEND_AS,
        PermissionType_SEND_ON_BEHALF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PermissionType = PermissionType'
  { fromPermissionType ::
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

pattern PermissionType_FULL_ACCESS :: PermissionType
pattern PermissionType_FULL_ACCESS = PermissionType' "FULL_ACCESS"

pattern PermissionType_SEND_AS :: PermissionType
pattern PermissionType_SEND_AS = PermissionType' "SEND_AS"

pattern PermissionType_SEND_ON_BEHALF :: PermissionType
pattern PermissionType_SEND_ON_BEHALF = PermissionType' "SEND_ON_BEHALF"

{-# COMPLETE
  PermissionType_FULL_ACCESS,
  PermissionType_SEND_AS,
  PermissionType_SEND_ON_BEHALF,
  PermissionType'
  #-}
