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
-- Module      : Network.AWS.ECS.Types.DesiredStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DesiredStatus
  ( DesiredStatus
      ( ..,
        DesiredStatus_PENDING,
        DesiredStatus_RUNNING,
        DesiredStatus_STOPPED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DesiredStatus = DesiredStatus'
  { fromDesiredStatus ::
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

pattern DesiredStatus_PENDING :: DesiredStatus
pattern DesiredStatus_PENDING = DesiredStatus' "PENDING"

pattern DesiredStatus_RUNNING :: DesiredStatus
pattern DesiredStatus_RUNNING = DesiredStatus' "RUNNING"

pattern DesiredStatus_STOPPED :: DesiredStatus
pattern DesiredStatus_STOPPED = DesiredStatus' "STOPPED"

{-# COMPLETE
  DesiredStatus_PENDING,
  DesiredStatus_RUNNING,
  DesiredStatus_STOPPED,
  DesiredStatus'
  #-}
