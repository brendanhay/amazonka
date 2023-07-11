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
-- Module      : Amazonka.SSM.Types.AssociationExecutionFilterKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AssociationExecutionFilterKey
  ( AssociationExecutionFilterKey
      ( ..,
        AssociationExecutionFilterKey_CreatedTime,
        AssociationExecutionFilterKey_ExecutionId,
        AssociationExecutionFilterKey_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssociationExecutionFilterKey = AssociationExecutionFilterKey'
  { fromAssociationExecutionFilterKey ::
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

pattern AssociationExecutionFilterKey_CreatedTime :: AssociationExecutionFilterKey
pattern AssociationExecutionFilterKey_CreatedTime = AssociationExecutionFilterKey' "CreatedTime"

pattern AssociationExecutionFilterKey_ExecutionId :: AssociationExecutionFilterKey
pattern AssociationExecutionFilterKey_ExecutionId = AssociationExecutionFilterKey' "ExecutionId"

pattern AssociationExecutionFilterKey_Status :: AssociationExecutionFilterKey
pattern AssociationExecutionFilterKey_Status = AssociationExecutionFilterKey' "Status"

{-# COMPLETE
  AssociationExecutionFilterKey_CreatedTime,
  AssociationExecutionFilterKey_ExecutionId,
  AssociationExecutionFilterKey_Status,
  AssociationExecutionFilterKey'
  #-}
