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
-- Module      : Network.AWS.ServiceCatalog.Types.CopyProductStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.CopyProductStatus
  ( CopyProductStatus
      ( ..,
        CopyProductStatus_FAILED,
        CopyProductStatus_IN_PROGRESS,
        CopyProductStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CopyProductStatus = CopyProductStatus'
  { fromCopyProductStatus ::
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

pattern CopyProductStatus_FAILED :: CopyProductStatus
pattern CopyProductStatus_FAILED = CopyProductStatus' "FAILED"

pattern CopyProductStatus_IN_PROGRESS :: CopyProductStatus
pattern CopyProductStatus_IN_PROGRESS = CopyProductStatus' "IN_PROGRESS"

pattern CopyProductStatus_SUCCEEDED :: CopyProductStatus
pattern CopyProductStatus_SUCCEEDED = CopyProductStatus' "SUCCEEDED"

{-# COMPLETE
  CopyProductStatus_FAILED,
  CopyProductStatus_IN_PROGRESS,
  CopyProductStatus_SUCCEEDED,
  CopyProductStatus'
  #-}
