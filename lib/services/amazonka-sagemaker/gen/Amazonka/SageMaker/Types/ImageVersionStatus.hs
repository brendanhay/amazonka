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
-- Module      : Amazonka.SageMaker.Types.ImageVersionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ImageVersionStatus
  ( ImageVersionStatus
      ( ..,
        ImageVersionStatus_CREATED,
        ImageVersionStatus_CREATE_FAILED,
        ImageVersionStatus_CREATING,
        ImageVersionStatus_DELETE_FAILED,
        ImageVersionStatus_DELETING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImageVersionStatus = ImageVersionStatus'
  { fromImageVersionStatus ::
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

pattern ImageVersionStatus_CREATED :: ImageVersionStatus
pattern ImageVersionStatus_CREATED = ImageVersionStatus' "CREATED"

pattern ImageVersionStatus_CREATE_FAILED :: ImageVersionStatus
pattern ImageVersionStatus_CREATE_FAILED = ImageVersionStatus' "CREATE_FAILED"

pattern ImageVersionStatus_CREATING :: ImageVersionStatus
pattern ImageVersionStatus_CREATING = ImageVersionStatus' "CREATING"

pattern ImageVersionStatus_DELETE_FAILED :: ImageVersionStatus
pattern ImageVersionStatus_DELETE_FAILED = ImageVersionStatus' "DELETE_FAILED"

pattern ImageVersionStatus_DELETING :: ImageVersionStatus
pattern ImageVersionStatus_DELETING = ImageVersionStatus' "DELETING"

{-# COMPLETE
  ImageVersionStatus_CREATED,
  ImageVersionStatus_CREATE_FAILED,
  ImageVersionStatus_CREATING,
  ImageVersionStatus_DELETE_FAILED,
  ImageVersionStatus_DELETING,
  ImageVersionStatus'
  #-}
