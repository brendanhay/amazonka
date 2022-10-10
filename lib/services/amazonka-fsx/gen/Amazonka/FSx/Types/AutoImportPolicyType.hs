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
-- Module      : Amazonka.FSx.Types.AutoImportPolicyType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.AutoImportPolicyType
  ( AutoImportPolicyType
      ( ..,
        AutoImportPolicyType_NEW,
        AutoImportPolicyType_NEW_CHANGED,
        AutoImportPolicyType_NEW_CHANGED_DELETED,
        AutoImportPolicyType_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AutoImportPolicyType = AutoImportPolicyType'
  { fromAutoImportPolicyType ::
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

pattern AutoImportPolicyType_NEW :: AutoImportPolicyType
pattern AutoImportPolicyType_NEW = AutoImportPolicyType' "NEW"

pattern AutoImportPolicyType_NEW_CHANGED :: AutoImportPolicyType
pattern AutoImportPolicyType_NEW_CHANGED = AutoImportPolicyType' "NEW_CHANGED"

pattern AutoImportPolicyType_NEW_CHANGED_DELETED :: AutoImportPolicyType
pattern AutoImportPolicyType_NEW_CHANGED_DELETED = AutoImportPolicyType' "NEW_CHANGED_DELETED"

pattern AutoImportPolicyType_NONE :: AutoImportPolicyType
pattern AutoImportPolicyType_NONE = AutoImportPolicyType' "NONE"

{-# COMPLETE
  AutoImportPolicyType_NEW,
  AutoImportPolicyType_NEW_CHANGED,
  AutoImportPolicyType_NEW_CHANGED_DELETED,
  AutoImportPolicyType_NONE,
  AutoImportPolicyType'
  #-}
