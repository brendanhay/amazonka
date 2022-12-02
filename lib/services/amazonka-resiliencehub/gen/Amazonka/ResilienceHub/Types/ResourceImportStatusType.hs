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
-- Module      : Amazonka.ResilienceHub.Types.ResourceImportStatusType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ResourceImportStatusType
  ( ResourceImportStatusType
      ( ..,
        ResourceImportStatusType_Failed,
        ResourceImportStatusType_InProgress,
        ResourceImportStatusType_Pending,
        ResourceImportStatusType_Success
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceImportStatusType = ResourceImportStatusType'
  { fromResourceImportStatusType ::
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

pattern ResourceImportStatusType_Failed :: ResourceImportStatusType
pattern ResourceImportStatusType_Failed = ResourceImportStatusType' "Failed"

pattern ResourceImportStatusType_InProgress :: ResourceImportStatusType
pattern ResourceImportStatusType_InProgress = ResourceImportStatusType' "InProgress"

pattern ResourceImportStatusType_Pending :: ResourceImportStatusType
pattern ResourceImportStatusType_Pending = ResourceImportStatusType' "Pending"

pattern ResourceImportStatusType_Success :: ResourceImportStatusType
pattern ResourceImportStatusType_Success = ResourceImportStatusType' "Success"

{-# COMPLETE
  ResourceImportStatusType_Failed,
  ResourceImportStatusType_InProgress,
  ResourceImportStatusType_Pending,
  ResourceImportStatusType_Success,
  ResourceImportStatusType'
  #-}
