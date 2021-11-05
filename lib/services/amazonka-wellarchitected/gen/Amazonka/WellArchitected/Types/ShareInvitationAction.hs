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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

-- | Share invitation action taken by contributor.
newtype ShareInvitationAction = ShareInvitationAction'
  { fromShareInvitationAction ::
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

pattern ShareInvitationAction_ACCEPT :: ShareInvitationAction
pattern ShareInvitationAction_ACCEPT = ShareInvitationAction' "ACCEPT"

pattern ShareInvitationAction_REJECT :: ShareInvitationAction
pattern ShareInvitationAction_REJECT = ShareInvitationAction' "REJECT"

{-# COMPLETE
  ShareInvitationAction_ACCEPT,
  ShareInvitationAction_REJECT,
  ShareInvitationAction'
  #-}
