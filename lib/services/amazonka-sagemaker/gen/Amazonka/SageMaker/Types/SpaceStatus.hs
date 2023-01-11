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
-- Module      : Amazonka.SageMaker.Types.SpaceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SpaceStatus
  ( SpaceStatus
      ( ..,
        SpaceStatus_Delete_Failed,
        SpaceStatus_Deleting,
        SpaceStatus_Failed,
        SpaceStatus_InService,
        SpaceStatus_Pending,
        SpaceStatus_Update_Failed,
        SpaceStatus_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SpaceStatus = SpaceStatus'
  { fromSpaceStatus ::
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

pattern SpaceStatus_Delete_Failed :: SpaceStatus
pattern SpaceStatus_Delete_Failed = SpaceStatus' "Delete_Failed"

pattern SpaceStatus_Deleting :: SpaceStatus
pattern SpaceStatus_Deleting = SpaceStatus' "Deleting"

pattern SpaceStatus_Failed :: SpaceStatus
pattern SpaceStatus_Failed = SpaceStatus' "Failed"

pattern SpaceStatus_InService :: SpaceStatus
pattern SpaceStatus_InService = SpaceStatus' "InService"

pattern SpaceStatus_Pending :: SpaceStatus
pattern SpaceStatus_Pending = SpaceStatus' "Pending"

pattern SpaceStatus_Update_Failed :: SpaceStatus
pattern SpaceStatus_Update_Failed = SpaceStatus' "Update_Failed"

pattern SpaceStatus_Updating :: SpaceStatus
pattern SpaceStatus_Updating = SpaceStatus' "Updating"

{-# COMPLETE
  SpaceStatus_Delete_Failed,
  SpaceStatus_Deleting,
  SpaceStatus_Failed,
  SpaceStatus_InService,
  SpaceStatus_Pending,
  SpaceStatus_Update_Failed,
  SpaceStatus_Updating,
  SpaceStatus'
  #-}
