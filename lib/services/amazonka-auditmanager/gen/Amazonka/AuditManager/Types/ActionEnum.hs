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
-- Module      : Amazonka.AuditManager.Types.ActionEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ActionEnum
  ( ActionEnum
      ( ..,
        ActionEnum_ACTIVE,
        ActionEnum_CREATE,
        ActionEnum_DELETE,
        ActionEnum_IMPORT_EVIDENCE,
        ActionEnum_INACTIVE,
        ActionEnum_REVIEWED,
        ActionEnum_UNDER_REVIEW,
        ActionEnum_UPDATE_METADATA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionEnum = ActionEnum'
  { fromActionEnum ::
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

pattern ActionEnum_ACTIVE :: ActionEnum
pattern ActionEnum_ACTIVE = ActionEnum' "ACTIVE"

pattern ActionEnum_CREATE :: ActionEnum
pattern ActionEnum_CREATE = ActionEnum' "CREATE"

pattern ActionEnum_DELETE :: ActionEnum
pattern ActionEnum_DELETE = ActionEnum' "DELETE"

pattern ActionEnum_IMPORT_EVIDENCE :: ActionEnum
pattern ActionEnum_IMPORT_EVIDENCE = ActionEnum' "IMPORT_EVIDENCE"

pattern ActionEnum_INACTIVE :: ActionEnum
pattern ActionEnum_INACTIVE = ActionEnum' "INACTIVE"

pattern ActionEnum_REVIEWED :: ActionEnum
pattern ActionEnum_REVIEWED = ActionEnum' "REVIEWED"

pattern ActionEnum_UNDER_REVIEW :: ActionEnum
pattern ActionEnum_UNDER_REVIEW = ActionEnum' "UNDER_REVIEW"

pattern ActionEnum_UPDATE_METADATA :: ActionEnum
pattern ActionEnum_UPDATE_METADATA = ActionEnum' "UPDATE_METADATA"

{-# COMPLETE
  ActionEnum_ACTIVE,
  ActionEnum_CREATE,
  ActionEnum_DELETE,
  ActionEnum_IMPORT_EVIDENCE,
  ActionEnum_INACTIVE,
  ActionEnum_REVIEWED,
  ActionEnum_UNDER_REVIEW,
  ActionEnum_UPDATE_METADATA,
  ActionEnum'
  #-}
