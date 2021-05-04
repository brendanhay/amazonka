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
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTableStatus
  ( GlobalTableStatus
      ( ..,
        GlobalTableStatus_ACTIVE,
        GlobalTableStatus_CREATING,
        GlobalTableStatus_DELETING,
        GlobalTableStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype GlobalTableStatus = GlobalTableStatus'
  { fromGlobalTableStatus ::
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

pattern GlobalTableStatus_ACTIVE :: GlobalTableStatus
pattern GlobalTableStatus_ACTIVE = GlobalTableStatus' "ACTIVE"

pattern GlobalTableStatus_CREATING :: GlobalTableStatus
pattern GlobalTableStatus_CREATING = GlobalTableStatus' "CREATING"

pattern GlobalTableStatus_DELETING :: GlobalTableStatus
pattern GlobalTableStatus_DELETING = GlobalTableStatus' "DELETING"

pattern GlobalTableStatus_UPDATING :: GlobalTableStatus
pattern GlobalTableStatus_UPDATING = GlobalTableStatus' "UPDATING"

{-# COMPLETE
  GlobalTableStatus_ACTIVE,
  GlobalTableStatus_CREATING,
  GlobalTableStatus_DELETING,
  GlobalTableStatus_UPDATING,
  GlobalTableStatus'
  #-}
