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
-- Module      : Network.AWS.MediaStore.Types.ContainerStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.ContainerStatus
  ( ContainerStatus
      ( ..,
        ContainerStatus_ACTIVE,
        ContainerStatus_CREATING,
        ContainerStatus_DELETING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ContainerStatus = ContainerStatus'
  { fromContainerStatus ::
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

pattern ContainerStatus_ACTIVE :: ContainerStatus
pattern ContainerStatus_ACTIVE = ContainerStatus' "ACTIVE"

pattern ContainerStatus_CREATING :: ContainerStatus
pattern ContainerStatus_CREATING = ContainerStatus' "CREATING"

pattern ContainerStatus_DELETING :: ContainerStatus
pattern ContainerStatus_DELETING = ContainerStatus' "DELETING"

{-# COMPLETE
  ContainerStatus_ACTIVE,
  ContainerStatus_CREATING,
  ContainerStatus_DELETING,
  ContainerStatus'
  #-}
