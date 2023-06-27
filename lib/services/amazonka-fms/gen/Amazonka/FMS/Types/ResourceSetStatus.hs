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
-- Module      : Amazonka.FMS.Types.ResourceSetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ResourceSetStatus
  ( ResourceSetStatus
      ( ..,
        ResourceSetStatus_ACTIVE,
        ResourceSetStatus_OUT_OF_ADMIN_SCOPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceSetStatus = ResourceSetStatus'
  { fromResourceSetStatus ::
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

pattern ResourceSetStatus_ACTIVE :: ResourceSetStatus
pattern ResourceSetStatus_ACTIVE = ResourceSetStatus' "ACTIVE"

pattern ResourceSetStatus_OUT_OF_ADMIN_SCOPE :: ResourceSetStatus
pattern ResourceSetStatus_OUT_OF_ADMIN_SCOPE = ResourceSetStatus' "OUT_OF_ADMIN_SCOPE"

{-# COMPLETE
  ResourceSetStatus_ACTIVE,
  ResourceSetStatus_OUT_OF_ADMIN_SCOPE,
  ResourceSetStatus'
  #-}
