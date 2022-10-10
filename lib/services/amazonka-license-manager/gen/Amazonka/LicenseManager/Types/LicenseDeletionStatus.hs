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
-- Module      : Amazonka.LicenseManager.Types.LicenseDeletionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseDeletionStatus
  ( LicenseDeletionStatus
      ( ..,
        LicenseDeletionStatus_DELETED,
        LicenseDeletionStatus_PENDING_DELETE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LicenseDeletionStatus = LicenseDeletionStatus'
  { fromLicenseDeletionStatus ::
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

pattern LicenseDeletionStatus_DELETED :: LicenseDeletionStatus
pattern LicenseDeletionStatus_DELETED = LicenseDeletionStatus' "DELETED"

pattern LicenseDeletionStatus_PENDING_DELETE :: LicenseDeletionStatus
pattern LicenseDeletionStatus_PENDING_DELETE = LicenseDeletionStatus' "PENDING_DELETE"

{-# COMPLETE
  LicenseDeletionStatus_DELETED,
  LicenseDeletionStatus_PENDING_DELETE,
  LicenseDeletionStatus'
  #-}
