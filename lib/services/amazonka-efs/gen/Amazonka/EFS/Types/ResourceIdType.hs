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
-- Module      : Amazonka.EFS.Types.ResourceIdType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.ResourceIdType
  ( ResourceIdType
      ( ..,
        ResourceIdType_LONG_ID,
        ResourceIdType_SHORT_ID
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A preference indicating a choice to use 63bit\/32bit IDs for all
-- applicable resources.
newtype ResourceIdType = ResourceIdType'
  { fromResourceIdType ::
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

pattern ResourceIdType_LONG_ID :: ResourceIdType
pattern ResourceIdType_LONG_ID = ResourceIdType' "LONG_ID"

pattern ResourceIdType_SHORT_ID :: ResourceIdType
pattern ResourceIdType_SHORT_ID = ResourceIdType' "SHORT_ID"

{-# COMPLETE
  ResourceIdType_LONG_ID,
  ResourceIdType_SHORT_ID,
  ResourceIdType'
  #-}
