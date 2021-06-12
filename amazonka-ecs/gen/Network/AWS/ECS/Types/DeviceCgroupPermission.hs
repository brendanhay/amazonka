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
-- Module      : Network.AWS.ECS.Types.DeviceCgroupPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeviceCgroupPermission
  ( DeviceCgroupPermission
      ( ..,
        DeviceCgroupPermission_Mknod,
        DeviceCgroupPermission_Read,
        DeviceCgroupPermission_Write
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DeviceCgroupPermission = DeviceCgroupPermission'
  { fromDeviceCgroupPermission ::
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

pattern DeviceCgroupPermission_Mknod :: DeviceCgroupPermission
pattern DeviceCgroupPermission_Mknod = DeviceCgroupPermission' "mknod"

pattern DeviceCgroupPermission_Read :: DeviceCgroupPermission
pattern DeviceCgroupPermission_Read = DeviceCgroupPermission' "read"

pattern DeviceCgroupPermission_Write :: DeviceCgroupPermission
pattern DeviceCgroupPermission_Write = DeviceCgroupPermission' "write"

{-# COMPLETE
  DeviceCgroupPermission_Mknod,
  DeviceCgroupPermission_Read,
  DeviceCgroupPermission_Write,
  DeviceCgroupPermission'
  #-}
