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
-- Module      : Amazonka.ServiceCatalog.Types.ShareStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ShareStatus
  ( ShareStatus
      ( ..,
        ShareStatus_COMPLETED,
        ShareStatus_COMPLETED_WITH_ERRORS,
        ShareStatus_ERROR,
        ShareStatus_IN_PROGRESS,
        ShareStatus_NOT_STARTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ShareStatus = ShareStatus'
  { fromShareStatus ::
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

pattern ShareStatus_COMPLETED :: ShareStatus
pattern ShareStatus_COMPLETED = ShareStatus' "COMPLETED"

pattern ShareStatus_COMPLETED_WITH_ERRORS :: ShareStatus
pattern ShareStatus_COMPLETED_WITH_ERRORS = ShareStatus' "COMPLETED_WITH_ERRORS"

pattern ShareStatus_ERROR :: ShareStatus
pattern ShareStatus_ERROR = ShareStatus' "ERROR"

pattern ShareStatus_IN_PROGRESS :: ShareStatus
pattern ShareStatus_IN_PROGRESS = ShareStatus' "IN_PROGRESS"

pattern ShareStatus_NOT_STARTED :: ShareStatus
pattern ShareStatus_NOT_STARTED = ShareStatus' "NOT_STARTED"

{-# COMPLETE
  ShareStatus_COMPLETED,
  ShareStatus_COMPLETED_WITH_ERRORS,
  ShareStatus_ERROR,
  ShareStatus_IN_PROGRESS,
  ShareStatus_NOT_STARTED,
  ShareStatus'
  #-}
