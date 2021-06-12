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
-- Module      : Network.AWS.ServiceCatalog.Types.ShareStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ShareStatus
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

import qualified Network.AWS.Core as Core

newtype ShareStatus = ShareStatus'
  { fromShareStatus ::
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
