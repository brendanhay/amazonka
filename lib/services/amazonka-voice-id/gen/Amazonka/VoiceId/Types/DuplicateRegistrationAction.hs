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
-- Module      : Amazonka.VoiceId.Types.DuplicateRegistrationAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.DuplicateRegistrationAction
  ( DuplicateRegistrationAction
      ( ..,
        DuplicateRegistrationAction_REGISTER_AS_NEW,
        DuplicateRegistrationAction_SKIP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DuplicateRegistrationAction = DuplicateRegistrationAction'
  { fromDuplicateRegistrationAction ::
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

pattern DuplicateRegistrationAction_REGISTER_AS_NEW :: DuplicateRegistrationAction
pattern DuplicateRegistrationAction_REGISTER_AS_NEW = DuplicateRegistrationAction' "REGISTER_AS_NEW"

pattern DuplicateRegistrationAction_SKIP :: DuplicateRegistrationAction
pattern DuplicateRegistrationAction_SKIP = DuplicateRegistrationAction' "SKIP"

{-# COMPLETE
  DuplicateRegistrationAction_REGISTER_AS_NEW,
  DuplicateRegistrationAction_SKIP,
  DuplicateRegistrationAction'
  #-}
