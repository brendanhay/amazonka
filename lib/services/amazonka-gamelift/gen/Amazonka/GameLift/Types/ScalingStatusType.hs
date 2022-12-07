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
-- Module      : Amazonka.GameLift.Types.ScalingStatusType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.ScalingStatusType
  ( ScalingStatusType
      ( ..,
        ScalingStatusType_ACTIVE,
        ScalingStatusType_DELETED,
        ScalingStatusType_DELETE_REQUESTED,
        ScalingStatusType_DELETING,
        ScalingStatusType_ERROR,
        ScalingStatusType_UPDATE_REQUESTED,
        ScalingStatusType_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScalingStatusType = ScalingStatusType'
  { fromScalingStatusType ::
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

pattern ScalingStatusType_ACTIVE :: ScalingStatusType
pattern ScalingStatusType_ACTIVE = ScalingStatusType' "ACTIVE"

pattern ScalingStatusType_DELETED :: ScalingStatusType
pattern ScalingStatusType_DELETED = ScalingStatusType' "DELETED"

pattern ScalingStatusType_DELETE_REQUESTED :: ScalingStatusType
pattern ScalingStatusType_DELETE_REQUESTED = ScalingStatusType' "DELETE_REQUESTED"

pattern ScalingStatusType_DELETING :: ScalingStatusType
pattern ScalingStatusType_DELETING = ScalingStatusType' "DELETING"

pattern ScalingStatusType_ERROR :: ScalingStatusType
pattern ScalingStatusType_ERROR = ScalingStatusType' "ERROR"

pattern ScalingStatusType_UPDATE_REQUESTED :: ScalingStatusType
pattern ScalingStatusType_UPDATE_REQUESTED = ScalingStatusType' "UPDATE_REQUESTED"

pattern ScalingStatusType_UPDATING :: ScalingStatusType
pattern ScalingStatusType_UPDATING = ScalingStatusType' "UPDATING"

{-# COMPLETE
  ScalingStatusType_ACTIVE,
  ScalingStatusType_DELETED,
  ScalingStatusType_DELETE_REQUESTED,
  ScalingStatusType_DELETING,
  ScalingStatusType_ERROR,
  ScalingStatusType_UPDATE_REQUESTED,
  ScalingStatusType_UPDATING,
  ScalingStatusType'
  #-}
