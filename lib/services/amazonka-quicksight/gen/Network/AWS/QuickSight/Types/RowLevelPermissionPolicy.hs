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
-- Module      : Network.AWS.QuickSight.Types.RowLevelPermissionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.RowLevelPermissionPolicy
  ( RowLevelPermissionPolicy
      ( ..,
        RowLevelPermissionPolicy_DENY_ACCESS,
        RowLevelPermissionPolicy_GRANT_ACCESS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RowLevelPermissionPolicy = RowLevelPermissionPolicy'
  { fromRowLevelPermissionPolicy ::
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

pattern RowLevelPermissionPolicy_DENY_ACCESS :: RowLevelPermissionPolicy
pattern RowLevelPermissionPolicy_DENY_ACCESS = RowLevelPermissionPolicy' "DENY_ACCESS"

pattern RowLevelPermissionPolicy_GRANT_ACCESS :: RowLevelPermissionPolicy
pattern RowLevelPermissionPolicy_GRANT_ACCESS = RowLevelPermissionPolicy' "GRANT_ACCESS"

{-# COMPLETE
  RowLevelPermissionPolicy_DENY_ACCESS,
  RowLevelPermissionPolicy_GRANT_ACCESS,
  RowLevelPermissionPolicy'
  #-}
