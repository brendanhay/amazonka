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
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformStatus
  ( PlatformStatus
      ( ..,
        PlatformStatus_Creating,
        PlatformStatus_Deleted,
        PlatformStatus_Deleting,
        PlatformStatus_Failed,
        PlatformStatus_Ready
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype PlatformStatus = PlatformStatus'
  { fromPlatformStatus ::
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

pattern PlatformStatus_Creating :: PlatformStatus
pattern PlatformStatus_Creating = PlatformStatus' "Creating"

pattern PlatformStatus_Deleted :: PlatformStatus
pattern PlatformStatus_Deleted = PlatformStatus' "Deleted"

pattern PlatformStatus_Deleting :: PlatformStatus
pattern PlatformStatus_Deleting = PlatformStatus' "Deleting"

pattern PlatformStatus_Failed :: PlatformStatus
pattern PlatformStatus_Failed = PlatformStatus' "Failed"

pattern PlatformStatus_Ready :: PlatformStatus
pattern PlatformStatus_Ready = PlatformStatus' "Ready"

{-# COMPLETE
  PlatformStatus_Creating,
  PlatformStatus_Deleted,
  PlatformStatus_Deleting,
  PlatformStatus_Failed,
  PlatformStatus_Ready,
  PlatformStatus'
  #-}
