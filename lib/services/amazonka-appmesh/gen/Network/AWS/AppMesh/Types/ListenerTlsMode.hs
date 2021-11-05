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
-- Module      : Network.AWS.AppMesh.Types.ListenerTlsMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.ListenerTlsMode
  ( ListenerTlsMode
      ( ..,
        ListenerTlsMode_DISABLED,
        ListenerTlsMode_PERMISSIVE,
        ListenerTlsMode_STRICT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ListenerTlsMode = ListenerTlsMode'
  { fromListenerTlsMode ::
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

pattern ListenerTlsMode_DISABLED :: ListenerTlsMode
pattern ListenerTlsMode_DISABLED = ListenerTlsMode' "DISABLED"

pattern ListenerTlsMode_PERMISSIVE :: ListenerTlsMode
pattern ListenerTlsMode_PERMISSIVE = ListenerTlsMode' "PERMISSIVE"

pattern ListenerTlsMode_STRICT :: ListenerTlsMode
pattern ListenerTlsMode_STRICT = ListenerTlsMode' "STRICT"

{-# COMPLETE
  ListenerTlsMode_DISABLED,
  ListenerTlsMode_PERMISSIVE,
  ListenerTlsMode_STRICT,
  ListenerTlsMode'
  #-}
