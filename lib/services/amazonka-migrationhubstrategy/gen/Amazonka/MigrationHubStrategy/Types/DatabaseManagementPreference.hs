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
-- Module      : Amazonka.MigrationHubStrategy.Types.DatabaseManagementPreference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.DatabaseManagementPreference
  ( DatabaseManagementPreference
      ( ..,
        DatabaseManagementPreference_AWS_managed,
        DatabaseManagementPreference_No_preference,
        DatabaseManagementPreference_Self_manage
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DatabaseManagementPreference = DatabaseManagementPreference'
  { fromDatabaseManagementPreference ::
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

pattern DatabaseManagementPreference_AWS_managed :: DatabaseManagementPreference
pattern DatabaseManagementPreference_AWS_managed = DatabaseManagementPreference' "AWS-managed"

pattern DatabaseManagementPreference_No_preference :: DatabaseManagementPreference
pattern DatabaseManagementPreference_No_preference = DatabaseManagementPreference' "No preference"

pattern DatabaseManagementPreference_Self_manage :: DatabaseManagementPreference
pattern DatabaseManagementPreference_Self_manage = DatabaseManagementPreference' "Self-manage"

{-# COMPLETE
  DatabaseManagementPreference_AWS_managed,
  DatabaseManagementPreference_No_preference,
  DatabaseManagementPreference_Self_manage,
  DatabaseManagementPreference'
  #-}
