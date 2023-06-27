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
-- Module      : Amazonka.Inspector2.Types.FindingType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FindingType
  ( FindingType
      ( ..,
        FindingType_CODE_VULNERABILITY,
        FindingType_NETWORK_REACHABILITY,
        FindingType_PACKAGE_VULNERABILITY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FindingType = FindingType'
  { fromFindingType ::
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

pattern FindingType_CODE_VULNERABILITY :: FindingType
pattern FindingType_CODE_VULNERABILITY = FindingType' "CODE_VULNERABILITY"

pattern FindingType_NETWORK_REACHABILITY :: FindingType
pattern FindingType_NETWORK_REACHABILITY = FindingType' "NETWORK_REACHABILITY"

pattern FindingType_PACKAGE_VULNERABILITY :: FindingType
pattern FindingType_PACKAGE_VULNERABILITY = FindingType' "PACKAGE_VULNERABILITY"

{-# COMPLETE
  FindingType_CODE_VULNERABILITY,
  FindingType_NETWORK_REACHABILITY,
  FindingType_PACKAGE_VULNERABILITY,
  FindingType'
  #-}
