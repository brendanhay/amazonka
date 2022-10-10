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
-- Module      : Amazonka.AuditManager.Types.ShareRequestAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ShareRequestAction
  ( ShareRequestAction
      ( ..,
        ShareRequestAction_ACCEPT,
        ShareRequestAction_DECLINE,
        ShareRequestAction_REVOKE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ShareRequestAction = ShareRequestAction'
  { fromShareRequestAction ::
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

pattern ShareRequestAction_ACCEPT :: ShareRequestAction
pattern ShareRequestAction_ACCEPT = ShareRequestAction' "ACCEPT"

pattern ShareRequestAction_DECLINE :: ShareRequestAction
pattern ShareRequestAction_DECLINE = ShareRequestAction' "DECLINE"

pattern ShareRequestAction_REVOKE :: ShareRequestAction
pattern ShareRequestAction_REVOKE = ShareRequestAction' "REVOKE"

{-# COMPLETE
  ShareRequestAction_ACCEPT,
  ShareRequestAction_DECLINE,
  ShareRequestAction_REVOKE,
  ShareRequestAction'
  #-}
