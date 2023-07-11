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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DuplicateRegistrationAction = DuplicateRegistrationAction'
  { fromDuplicateRegistrationAction ::
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

pattern DuplicateRegistrationAction_REGISTER_AS_NEW :: DuplicateRegistrationAction
pattern DuplicateRegistrationAction_REGISTER_AS_NEW = DuplicateRegistrationAction' "REGISTER_AS_NEW"

pattern DuplicateRegistrationAction_SKIP :: DuplicateRegistrationAction
pattern DuplicateRegistrationAction_SKIP = DuplicateRegistrationAction' "SKIP"

{-# COMPLETE
  DuplicateRegistrationAction_REGISTER_AS_NEW,
  DuplicateRegistrationAction_SKIP,
  DuplicateRegistrationAction'
  #-}
