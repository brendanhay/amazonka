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
-- Module      : Amazonka.Glue.Types.TransformStatusType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TransformStatusType
  ( TransformStatusType
      ( ..,
        TransformStatusType_DELETING,
        TransformStatusType_NOT_READY,
        TransformStatusType_READY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TransformStatusType = TransformStatusType'
  { fromTransformStatusType ::
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

pattern TransformStatusType_DELETING :: TransformStatusType
pattern TransformStatusType_DELETING = TransformStatusType' "DELETING"

pattern TransformStatusType_NOT_READY :: TransformStatusType
pattern TransformStatusType_NOT_READY = TransformStatusType' "NOT_READY"

pattern TransformStatusType_READY :: TransformStatusType
pattern TransformStatusType_READY = TransformStatusType' "READY"

{-# COMPLETE
  TransformStatusType_DELETING,
  TransformStatusType_NOT_READY,
  TransformStatusType_READY,
  TransformStatusType'
  #-}
