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
-- Module      : Amazonka.OpenSearch.Types.SkipUnavailableStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.SkipUnavailableStatus
  ( SkipUnavailableStatus
      ( ..,
        SkipUnavailableStatus_DISABLED,
        SkipUnavailableStatus_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Status of SkipUnavailable param for outbound connection.
--
-- -   __ENABLED__ - The SkipUnavailable param is enabled for the
--     connection.
--
-- -   __DISABLED__ - The SkipUnavailable param is disabled for the
--     connection.
newtype SkipUnavailableStatus = SkipUnavailableStatus'
  { fromSkipUnavailableStatus ::
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

pattern SkipUnavailableStatus_DISABLED :: SkipUnavailableStatus
pattern SkipUnavailableStatus_DISABLED = SkipUnavailableStatus' "DISABLED"

pattern SkipUnavailableStatus_ENABLED :: SkipUnavailableStatus
pattern SkipUnavailableStatus_ENABLED = SkipUnavailableStatus' "ENABLED"

{-# COMPLETE
  SkipUnavailableStatus_DISABLED,
  SkipUnavailableStatus_ENABLED,
  SkipUnavailableStatus'
  #-}
