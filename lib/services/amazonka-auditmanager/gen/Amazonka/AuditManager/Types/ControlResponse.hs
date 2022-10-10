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
-- Module      : Amazonka.AuditManager.Types.ControlResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlResponse
  ( ControlResponse
      ( ..,
        ControlResponse_AUTOMATE,
        ControlResponse_DEFER,
        ControlResponse_IGNORE,
        ControlResponse_MANUAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ControlResponse = ControlResponse'
  { fromControlResponse ::
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

pattern ControlResponse_AUTOMATE :: ControlResponse
pattern ControlResponse_AUTOMATE = ControlResponse' "AUTOMATE"

pattern ControlResponse_DEFER :: ControlResponse
pattern ControlResponse_DEFER = ControlResponse' "DEFER"

pattern ControlResponse_IGNORE :: ControlResponse
pattern ControlResponse_IGNORE = ControlResponse' "IGNORE"

pattern ControlResponse_MANUAL :: ControlResponse
pattern ControlResponse_MANUAL = ControlResponse' "MANUAL"

{-# COMPLETE
  ControlResponse_AUTOMATE,
  ControlResponse_DEFER,
  ControlResponse_IGNORE,
  ControlResponse_MANUAL,
  ControlResponse'
  #-}
