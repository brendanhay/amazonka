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
-- Module      : Amazonka.Route53AutoNaming.Types.OperationFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.OperationFilterName
  ( OperationFilterName
      ( ..,
        OperationFilterName_NAMESPACE_ID,
        OperationFilterName_SERVICE_ID,
        OperationFilterName_STATUS,
        OperationFilterName_TYPE,
        OperationFilterName_UPDATE_DATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OperationFilterName = OperationFilterName'
  { fromOperationFilterName ::
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

pattern OperationFilterName_NAMESPACE_ID :: OperationFilterName
pattern OperationFilterName_NAMESPACE_ID = OperationFilterName' "NAMESPACE_ID"

pattern OperationFilterName_SERVICE_ID :: OperationFilterName
pattern OperationFilterName_SERVICE_ID = OperationFilterName' "SERVICE_ID"

pattern OperationFilterName_STATUS :: OperationFilterName
pattern OperationFilterName_STATUS = OperationFilterName' "STATUS"

pattern OperationFilterName_TYPE :: OperationFilterName
pattern OperationFilterName_TYPE = OperationFilterName' "TYPE"

pattern OperationFilterName_UPDATE_DATE :: OperationFilterName
pattern OperationFilterName_UPDATE_DATE = OperationFilterName' "UPDATE_DATE"

{-# COMPLETE
  OperationFilterName_NAMESPACE_ID,
  OperationFilterName_SERVICE_ID,
  OperationFilterName_STATUS,
  OperationFilterName_TYPE,
  OperationFilterName_UPDATE_DATE,
  OperationFilterName'
  #-}
