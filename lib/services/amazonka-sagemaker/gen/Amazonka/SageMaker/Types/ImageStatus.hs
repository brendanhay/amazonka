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
-- Module      : Amazonka.SageMaker.Types.ImageStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ImageStatus
  ( ImageStatus
      ( ..,
        ImageStatus_CREATED,
        ImageStatus_CREATE_FAILED,
        ImageStatus_CREATING,
        ImageStatus_DELETE_FAILED,
        ImageStatus_DELETING,
        ImageStatus_UPDATE_FAILED,
        ImageStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImageStatus = ImageStatus'
  { fromImageStatus ::
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

pattern ImageStatus_CREATED :: ImageStatus
pattern ImageStatus_CREATED = ImageStatus' "CREATED"

pattern ImageStatus_CREATE_FAILED :: ImageStatus
pattern ImageStatus_CREATE_FAILED = ImageStatus' "CREATE_FAILED"

pattern ImageStatus_CREATING :: ImageStatus
pattern ImageStatus_CREATING = ImageStatus' "CREATING"

pattern ImageStatus_DELETE_FAILED :: ImageStatus
pattern ImageStatus_DELETE_FAILED = ImageStatus' "DELETE_FAILED"

pattern ImageStatus_DELETING :: ImageStatus
pattern ImageStatus_DELETING = ImageStatus' "DELETING"

pattern ImageStatus_UPDATE_FAILED :: ImageStatus
pattern ImageStatus_UPDATE_FAILED = ImageStatus' "UPDATE_FAILED"

pattern ImageStatus_UPDATING :: ImageStatus
pattern ImageStatus_UPDATING = ImageStatus' "UPDATING"

{-# COMPLETE
  ImageStatus_CREATED,
  ImageStatus_CREATE_FAILED,
  ImageStatus_CREATING,
  ImageStatus_DELETE_FAILED,
  ImageStatus_DELETING,
  ImageStatus_UPDATE_FAILED,
  ImageStatus_UPDATING,
  ImageStatus'
  #-}
