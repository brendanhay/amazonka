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
-- Module      : Amazonka.DirectoryService.Types.RadiusStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.RadiusStatus
  ( RadiusStatus
      ( ..,
        RadiusStatus_Completed,
        RadiusStatus_Creating,
        RadiusStatus_Failed
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RadiusStatus = RadiusStatus'
  { fromRadiusStatus ::
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

pattern RadiusStatus_Completed :: RadiusStatus
pattern RadiusStatus_Completed = RadiusStatus' "Completed"

pattern RadiusStatus_Creating :: RadiusStatus
pattern RadiusStatus_Creating = RadiusStatus' "Creating"

pattern RadiusStatus_Failed :: RadiusStatus
pattern RadiusStatus_Failed = RadiusStatus' "Failed"

{-# COMPLETE
  RadiusStatus_Completed,
  RadiusStatus_Creating,
  RadiusStatus_Failed,
  RadiusStatus'
  #-}
