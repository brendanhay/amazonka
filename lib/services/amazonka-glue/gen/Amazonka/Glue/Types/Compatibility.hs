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
-- Module      : Amazonka.Glue.Types.Compatibility
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Compatibility
  ( Compatibility
      ( ..,
        Compatibility_BACKWARD,
        Compatibility_BACKWARD_ALL,
        Compatibility_DISABLED,
        Compatibility_FORWARD,
        Compatibility_FORWARD_ALL,
        Compatibility_FULL,
        Compatibility_FULL_ALL,
        Compatibility_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Compatibility = Compatibility'
  { fromCompatibility ::
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

pattern Compatibility_BACKWARD :: Compatibility
pattern Compatibility_BACKWARD = Compatibility' "BACKWARD"

pattern Compatibility_BACKWARD_ALL :: Compatibility
pattern Compatibility_BACKWARD_ALL = Compatibility' "BACKWARD_ALL"

pattern Compatibility_DISABLED :: Compatibility
pattern Compatibility_DISABLED = Compatibility' "DISABLED"

pattern Compatibility_FORWARD :: Compatibility
pattern Compatibility_FORWARD = Compatibility' "FORWARD"

pattern Compatibility_FORWARD_ALL :: Compatibility
pattern Compatibility_FORWARD_ALL = Compatibility' "FORWARD_ALL"

pattern Compatibility_FULL :: Compatibility
pattern Compatibility_FULL = Compatibility' "FULL"

pattern Compatibility_FULL_ALL :: Compatibility
pattern Compatibility_FULL_ALL = Compatibility' "FULL_ALL"

pattern Compatibility_NONE :: Compatibility
pattern Compatibility_NONE = Compatibility' "NONE"

{-# COMPLETE
  Compatibility_BACKWARD,
  Compatibility_BACKWARD_ALL,
  Compatibility_DISABLED,
  Compatibility_FORWARD,
  Compatibility_FORWARD_ALL,
  Compatibility_FULL,
  Compatibility_FULL_ALL,
  Compatibility_NONE,
  Compatibility'
  #-}
