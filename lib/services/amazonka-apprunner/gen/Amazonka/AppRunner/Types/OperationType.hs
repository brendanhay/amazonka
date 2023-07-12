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
-- Module      : Amazonka.AppRunner.Types.OperationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.OperationType
  ( OperationType
      ( ..,
        OperationType_CREATE_SERVICE,
        OperationType_DELETE_SERVICE,
        OperationType_PAUSE_SERVICE,
        OperationType_RESUME_SERVICE,
        OperationType_START_DEPLOYMENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OperationType = OperationType'
  { fromOperationType ::
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

pattern OperationType_CREATE_SERVICE :: OperationType
pattern OperationType_CREATE_SERVICE = OperationType' "CREATE_SERVICE"

pattern OperationType_DELETE_SERVICE :: OperationType
pattern OperationType_DELETE_SERVICE = OperationType' "DELETE_SERVICE"

pattern OperationType_PAUSE_SERVICE :: OperationType
pattern OperationType_PAUSE_SERVICE = OperationType' "PAUSE_SERVICE"

pattern OperationType_RESUME_SERVICE :: OperationType
pattern OperationType_RESUME_SERVICE = OperationType' "RESUME_SERVICE"

pattern OperationType_START_DEPLOYMENT :: OperationType
pattern OperationType_START_DEPLOYMENT = OperationType' "START_DEPLOYMENT"

{-# COMPLETE
  OperationType_CREATE_SERVICE,
  OperationType_DELETE_SERVICE,
  OperationType_PAUSE_SERVICE,
  OperationType_RESUME_SERVICE,
  OperationType_START_DEPLOYMENT,
  OperationType'
  #-}
