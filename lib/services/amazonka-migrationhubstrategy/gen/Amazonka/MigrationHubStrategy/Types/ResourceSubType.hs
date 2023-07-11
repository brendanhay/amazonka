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
-- Module      : Amazonka.MigrationHubStrategy.Types.ResourceSubType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ResourceSubType
  ( ResourceSubType
      ( ..,
        ResourceSubType_Database,
        ResourceSubType_DatabaseProcess,
        ResourceSubType_Process
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceSubType = ResourceSubType'
  { fromResourceSubType ::
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

pattern ResourceSubType_Database :: ResourceSubType
pattern ResourceSubType_Database = ResourceSubType' "Database"

pattern ResourceSubType_DatabaseProcess :: ResourceSubType
pattern ResourceSubType_DatabaseProcess = ResourceSubType' "DatabaseProcess"

pattern ResourceSubType_Process :: ResourceSubType
pattern ResourceSubType_Process = ResourceSubType' "Process"

{-# COMPLETE
  ResourceSubType_Database,
  ResourceSubType_DatabaseProcess,
  ResourceSubType_Process,
  ResourceSubType'
  #-}
