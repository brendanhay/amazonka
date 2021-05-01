{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OTAUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OTAUpdateStatus
  ( OTAUpdateStatus
      ( ..,
        OTAUpdateStatus_CREATE_COMPLETE,
        OTAUpdateStatus_CREATE_FAILED,
        OTAUpdateStatus_CREATE_IN_PROGRESS,
        OTAUpdateStatus_CREATE_PENDING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype OTAUpdateStatus = OTAUpdateStatus'
  { fromOTAUpdateStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern OTAUpdateStatus_CREATE_COMPLETE :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_COMPLETE = OTAUpdateStatus' "CREATE_COMPLETE"

pattern OTAUpdateStatus_CREATE_FAILED :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_FAILED = OTAUpdateStatus' "CREATE_FAILED"

pattern OTAUpdateStatus_CREATE_IN_PROGRESS :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_IN_PROGRESS = OTAUpdateStatus' "CREATE_IN_PROGRESS"

pattern OTAUpdateStatus_CREATE_PENDING :: OTAUpdateStatus
pattern OTAUpdateStatus_CREATE_PENDING = OTAUpdateStatus' "CREATE_PENDING"

{-# COMPLETE
  OTAUpdateStatus_CREATE_COMPLETE,
  OTAUpdateStatus_CREATE_FAILED,
  OTAUpdateStatus_CREATE_IN_PROGRESS,
  OTAUpdateStatus_CREATE_PENDING,
  OTAUpdateStatus'
  #-}
