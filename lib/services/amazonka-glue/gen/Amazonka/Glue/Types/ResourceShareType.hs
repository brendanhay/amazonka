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
-- Module      : Amazonka.Glue.Types.ResourceShareType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ResourceShareType
  ( ResourceShareType
      ( ..,
        ResourceShareType_ALL,
        ResourceShareType_FEDERATED,
        ResourceShareType_FOREIGN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceShareType = ResourceShareType'
  { fromResourceShareType ::
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

pattern ResourceShareType_ALL :: ResourceShareType
pattern ResourceShareType_ALL = ResourceShareType' "ALL"

pattern ResourceShareType_FEDERATED :: ResourceShareType
pattern ResourceShareType_FEDERATED = ResourceShareType' "FEDERATED"

pattern ResourceShareType_FOREIGN :: ResourceShareType
pattern ResourceShareType_FOREIGN = ResourceShareType' "FOREIGN"

{-# COMPLETE
  ResourceShareType_ALL,
  ResourceShareType_FEDERATED,
  ResourceShareType_FOREIGN,
  ResourceShareType'
  #-}
