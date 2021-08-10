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
-- Module      : Network.AWS.WorkMail.Types.PermissionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.PermissionType
  ( PermissionType
      ( ..,
        PermissionType_FULL_ACCESS,
        PermissionType_SEND_AS,
        PermissionType_SEND_ON_BEHALF
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PermissionType = PermissionType'
  { fromPermissionType ::
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
