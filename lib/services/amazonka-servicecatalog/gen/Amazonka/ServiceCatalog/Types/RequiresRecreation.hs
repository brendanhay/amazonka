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
-- Module      : Amazonka.ServiceCatalog.Types.RequiresRecreation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.RequiresRecreation
  ( RequiresRecreation
      ( ..,
        RequiresRecreation_ALWAYS,
        RequiresRecreation_CONDITIONALLY,
        RequiresRecreation_NEVER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RequiresRecreation = RequiresRecreation'
  { fromRequiresRecreation ::
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

pattern RequiresRecreation_ALWAYS :: RequiresRecreation
pattern RequiresRecreation_ALWAYS = RequiresRecreation' "ALWAYS"

pattern RequiresRecreation_CONDITIONALLY :: RequiresRecreation
pattern RequiresRecreation_CONDITIONALLY = RequiresRecreation' "CONDITIONALLY"

pattern RequiresRecreation_NEVER :: RequiresRecreation
pattern RequiresRecreation_NEVER = RequiresRecreation' "NEVER"

{-# COMPLETE
  RequiresRecreation_ALWAYS,
  RequiresRecreation_CONDITIONALLY,
  RequiresRecreation_NEVER,
  RequiresRecreation'
  #-}
