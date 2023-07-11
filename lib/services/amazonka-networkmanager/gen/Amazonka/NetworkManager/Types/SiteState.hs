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
-- Module      : Amazonka.NetworkManager.Types.SiteState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.SiteState
  ( SiteState
      ( ..,
        SiteState_AVAILABLE,
        SiteState_DELETING,
        SiteState_PENDING,
        SiteState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SiteState = SiteState'
  { fromSiteState ::
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

pattern SiteState_AVAILABLE :: SiteState
pattern SiteState_AVAILABLE = SiteState' "AVAILABLE"

pattern SiteState_DELETING :: SiteState
pattern SiteState_DELETING = SiteState' "DELETING"

pattern SiteState_PENDING :: SiteState
pattern SiteState_PENDING = SiteState' "PENDING"

pattern SiteState_UPDATING :: SiteState
pattern SiteState_UPDATING = SiteState' "UPDATING"

{-# COMPLETE
  SiteState_AVAILABLE,
  SiteState_DELETING,
  SiteState_PENDING,
  SiteState_UPDATING,
  SiteState'
  #-}
