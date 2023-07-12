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
-- Module      : Amazonka.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
  ( Scte35SpliceInsertWebDeliveryAllowedBehavior
      ( ..,
        Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW,
        Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Scte35 Splice Insert Web Delivery Allowed Behavior
newtype Scte35SpliceInsertWebDeliveryAllowedBehavior = Scte35SpliceInsertWebDeliveryAllowedBehavior'
  { fromScte35SpliceInsertWebDeliveryAllowedBehavior ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW = Scte35SpliceInsertWebDeliveryAllowedBehavior' "FOLLOW"

pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE = Scte35SpliceInsertWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE
  Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW,
  Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE,
  Scte35SpliceInsertWebDeliveryAllowedBehavior'
  #-}
