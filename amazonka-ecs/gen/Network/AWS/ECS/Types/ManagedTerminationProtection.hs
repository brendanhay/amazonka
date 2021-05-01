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
-- Module      : Network.AWS.ECS.Types.ManagedTerminationProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ManagedTerminationProtection
  ( ManagedTerminationProtection
      ( ..,
        ManagedTerminationProtection_DISABLED,
        ManagedTerminationProtection_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ManagedTerminationProtection = ManagedTerminationProtection'
  { fromManagedTerminationProtection ::
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

pattern ManagedTerminationProtection_DISABLED :: ManagedTerminationProtection
pattern ManagedTerminationProtection_DISABLED = ManagedTerminationProtection' "DISABLED"

pattern ManagedTerminationProtection_ENABLED :: ManagedTerminationProtection
pattern ManagedTerminationProtection_ENABLED = ManagedTerminationProtection' "ENABLED"

{-# COMPLETE
  ManagedTerminationProtection_DISABLED,
  ManagedTerminationProtection_ENABLED,
  ManagedTerminationProtection'
  #-}
