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
-- Module      : Amazonka.MediaStore.Types.ContainerStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaStore.Types.ContainerStatus
  ( ContainerStatus
      ( ..,
        ContainerStatus_ACTIVE,
        ContainerStatus_CREATING,
        ContainerStatus_DELETING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ContainerStatus = ContainerStatus'
  { fromContainerStatus ::
      Core.Text
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
