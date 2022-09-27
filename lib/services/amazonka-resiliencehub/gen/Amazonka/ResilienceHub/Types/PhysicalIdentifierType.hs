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
-- Module      : Amazonka.ResilienceHub.Types.PhysicalIdentifierType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.PhysicalIdentifierType
  ( PhysicalIdentifierType
      ( ..,
        PhysicalIdentifierType_Arn,
        PhysicalIdentifierType_Native
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PhysicalIdentifierType = PhysicalIdentifierType'
  { fromPhysicalIdentifierType ::
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

pattern PhysicalIdentifierType_Arn :: PhysicalIdentifierType
pattern PhysicalIdentifierType_Arn = PhysicalIdentifierType' "Arn"

pattern PhysicalIdentifierType_Native :: PhysicalIdentifierType
pattern PhysicalIdentifierType_Native = PhysicalIdentifierType' "Native"

{-# COMPLETE
  PhysicalIdentifierType_Arn,
  PhysicalIdentifierType_Native,
  PhysicalIdentifierType'
  #-}
