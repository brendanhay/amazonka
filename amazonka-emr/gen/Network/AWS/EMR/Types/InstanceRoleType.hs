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
-- Module      : Network.AWS.EMR.Types.InstanceRoleType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceRoleType
  ( InstanceRoleType
      ( ..,
        InstanceRoleType_CORE,
        InstanceRoleType_MASTER,
        InstanceRoleType_TASK
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype InstanceRoleType = InstanceRoleType'
  { fromInstanceRoleType ::
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

pattern InstanceRoleType_CORE :: InstanceRoleType
pattern InstanceRoleType_CORE = InstanceRoleType' "CORE"

pattern InstanceRoleType_MASTER :: InstanceRoleType
pattern InstanceRoleType_MASTER = InstanceRoleType' "MASTER"

pattern InstanceRoleType_TASK :: InstanceRoleType
pattern InstanceRoleType_TASK = InstanceRoleType' "TASK"

{-# COMPLETE
  InstanceRoleType_CORE,
  InstanceRoleType_MASTER,
  InstanceRoleType_TASK,
  InstanceRoleType'
  #-}
