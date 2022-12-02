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
-- Module      : Amazonka.MachineLearning.Types.EntityStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.EntityStatus
  ( EntityStatus
      ( ..,
        EntityStatus_COMPLETED,
        EntityStatus_DELETED,
        EntityStatus_FAILED,
        EntityStatus_INPROGRESS,
        EntityStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object status with the following possible values:
--
-- -   @PENDING@
--
-- -   @INPROGRESS@
--
-- -   @FAILED@
--
-- -   @COMPLETED@
--
-- -   @DELETED@
newtype EntityStatus = EntityStatus'
  { fromEntityStatus ::
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

pattern EntityStatus_COMPLETED :: EntityStatus
pattern EntityStatus_COMPLETED = EntityStatus' "COMPLETED"

pattern EntityStatus_DELETED :: EntityStatus
pattern EntityStatus_DELETED = EntityStatus' "DELETED"

pattern EntityStatus_FAILED :: EntityStatus
pattern EntityStatus_FAILED = EntityStatus' "FAILED"

pattern EntityStatus_INPROGRESS :: EntityStatus
pattern EntityStatus_INPROGRESS = EntityStatus' "INPROGRESS"

pattern EntityStatus_PENDING :: EntityStatus
pattern EntityStatus_PENDING = EntityStatus' "PENDING"

{-# COMPLETE
  EntityStatus_COMPLETED,
  EntityStatus_DELETED,
  EntityStatus_FAILED,
  EntityStatus_INPROGRESS,
  EntityStatus_PENDING,
  EntityStatus'
  #-}
