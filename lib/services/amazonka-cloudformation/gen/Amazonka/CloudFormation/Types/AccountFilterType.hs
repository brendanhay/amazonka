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
-- Module      : Amazonka.CloudFormation.Types.AccountFilterType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.AccountFilterType
  ( AccountFilterType
      ( ..,
        AccountFilterType_DIFFERENCE,
        AccountFilterType_INTERSECTION,
        AccountFilterType_NONE,
        AccountFilterType_UNION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccountFilterType = AccountFilterType'
  { fromAccountFilterType ::
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

pattern AccountFilterType_DIFFERENCE :: AccountFilterType
pattern AccountFilterType_DIFFERENCE = AccountFilterType' "DIFFERENCE"

pattern AccountFilterType_INTERSECTION :: AccountFilterType
pattern AccountFilterType_INTERSECTION = AccountFilterType' "INTERSECTION"

pattern AccountFilterType_NONE :: AccountFilterType
pattern AccountFilterType_NONE = AccountFilterType' "NONE"

pattern AccountFilterType_UNION :: AccountFilterType
pattern AccountFilterType_UNION = AccountFilterType' "UNION"

{-# COMPLETE
  AccountFilterType_DIFFERENCE,
  AccountFilterType_INTERSECTION,
  AccountFilterType_NONE,
  AccountFilterType_UNION,
  AccountFilterType'
  #-}
