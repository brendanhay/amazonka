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
-- Module      : Network.AWS.MachineLearning.Types.EntityStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.EntityStatus
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

import qualified Network.AWS.Core as Core

-- | Object status with the following possible values:
--
-- -   @PENDING@
-- -   @INPROGRESS@
-- -   @FAILED@
-- -   @COMPLETED@
-- -   @DELETED@
newtype EntityStatus = EntityStatus'
  { fromEntityStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
