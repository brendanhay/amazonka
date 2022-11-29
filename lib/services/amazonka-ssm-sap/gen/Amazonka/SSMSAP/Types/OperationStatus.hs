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
-- Module      : Amazonka.SSMSAP.Types.OperationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.OperationStatus
  ( OperationStatus
      ( ..,
        OperationStatus_ERROR,
        OperationStatus_INPROGRESS,
        OperationStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OperationStatus = OperationStatus'
  { fromOperationStatus ::
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

pattern OperationStatus_ERROR :: OperationStatus
pattern OperationStatus_ERROR = OperationStatus' "ERROR"

pattern OperationStatus_INPROGRESS :: OperationStatus
pattern OperationStatus_INPROGRESS = OperationStatus' "INPROGRESS"

pattern OperationStatus_SUCCESS :: OperationStatus
pattern OperationStatus_SUCCESS = OperationStatus' "SUCCESS"

{-# COMPLETE
  OperationStatus_ERROR,
  OperationStatus_INPROGRESS,
  OperationStatus_SUCCESS,
  OperationStatus'
  #-}
