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
-- Module      : Network.AWS.Lambda.Types.LastUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LastUpdateStatus
  ( LastUpdateStatus
      ( ..,
        LastUpdateStatus_Failed,
        LastUpdateStatus_InProgress,
        LastUpdateStatus_Successful
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LastUpdateStatus = LastUpdateStatus'
  { fromLastUpdateStatus ::
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

pattern LastUpdateStatus_Failed :: LastUpdateStatus
pattern LastUpdateStatus_Failed = LastUpdateStatus' "Failed"

pattern LastUpdateStatus_InProgress :: LastUpdateStatus
pattern LastUpdateStatus_InProgress = LastUpdateStatus' "InProgress"

pattern LastUpdateStatus_Successful :: LastUpdateStatus
pattern LastUpdateStatus_Successful = LastUpdateStatus' "Successful"

{-# COMPLETE
  LastUpdateStatus_Failed,
  LastUpdateStatus_InProgress,
  LastUpdateStatus_Successful,
  LastUpdateStatus'
  #-}
