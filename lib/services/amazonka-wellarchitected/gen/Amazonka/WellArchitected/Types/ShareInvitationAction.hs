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
-- Module      : Amazonka.WellArchitected.Types.ShareInvitationAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ShareInvitationAction
  ( ShareInvitationAction
      ( ..,
        ShareInvitationAction_ACCEPT,
        ShareInvitationAction_REJECT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Share invitation action taken by contributor.
newtype ShareInvitationAction = ShareInvitationAction'
  { fromShareInvitationAction ::
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

pattern ShareInvitationAction_ACCEPT :: ShareInvitationAction
pattern ShareInvitationAction_ACCEPT = ShareInvitationAction' "ACCEPT"

pattern ShareInvitationAction_REJECT :: ShareInvitationAction
pattern ShareInvitationAction_REJECT = ShareInvitationAction' "REJECT"

{-# COMPLETE
  ShareInvitationAction_ACCEPT,
  ShareInvitationAction_REJECT,
  ShareInvitationAction'
  #-}
