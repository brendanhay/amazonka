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
-- Module      : Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
  ( PendingAutomaticFailoverStatus
      ( ..,
        PendingAutomaticFailoverStatus_Disabled,
        PendingAutomaticFailoverStatus_Enabled
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype PendingAutomaticFailoverStatus = PendingAutomaticFailoverStatus'
  { fromPendingAutomaticFailoverStatus ::
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

pattern PendingAutomaticFailoverStatus_Disabled :: PendingAutomaticFailoverStatus
pattern PendingAutomaticFailoverStatus_Disabled = PendingAutomaticFailoverStatus' "disabled"

pattern PendingAutomaticFailoverStatus_Enabled :: PendingAutomaticFailoverStatus
pattern PendingAutomaticFailoverStatus_Enabled = PendingAutomaticFailoverStatus' "enabled"

{-# COMPLETE
  PendingAutomaticFailoverStatus_Disabled,
  PendingAutomaticFailoverStatus_Enabled,
  PendingAutomaticFailoverStatus'
  #-}
