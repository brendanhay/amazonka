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
-- Module      : Amazonka.AWSHealth.Types.EntityStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EntityStatusCode
  ( EntityStatusCode
      ( ..,
        EntityStatusCode_IMPAIRED,
        EntityStatusCode_UNIMPAIRED,
        EntityStatusCode_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EntityStatusCode = EntityStatusCode'
  { fromEntityStatusCode ::
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

pattern EntityStatusCode_IMPAIRED :: EntityStatusCode
pattern EntityStatusCode_IMPAIRED = EntityStatusCode' "IMPAIRED"

pattern EntityStatusCode_UNIMPAIRED :: EntityStatusCode
pattern EntityStatusCode_UNIMPAIRED = EntityStatusCode' "UNIMPAIRED"

pattern EntityStatusCode_UNKNOWN :: EntityStatusCode
pattern EntityStatusCode_UNKNOWN = EntityStatusCode' "UNKNOWN"

{-# COMPLETE
  EntityStatusCode_IMPAIRED,
  EntityStatusCode_UNIMPAIRED,
  EntityStatusCode_UNKNOWN,
  EntityStatusCode'
  #-}
