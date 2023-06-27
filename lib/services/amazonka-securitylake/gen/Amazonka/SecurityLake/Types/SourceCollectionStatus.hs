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
-- Module      : Amazonka.SecurityLake.Types.SourceCollectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.SourceCollectionStatus
  ( SourceCollectionStatus
      ( ..,
        SourceCollectionStatus_COLLECTING,
        SourceCollectionStatus_MISCONFIGURED,
        SourceCollectionStatus_NOT_COLLECTING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceCollectionStatus = SourceCollectionStatus'
  { fromSourceCollectionStatus ::
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

pattern SourceCollectionStatus_COLLECTING :: SourceCollectionStatus
pattern SourceCollectionStatus_COLLECTING = SourceCollectionStatus' "COLLECTING"

pattern SourceCollectionStatus_MISCONFIGURED :: SourceCollectionStatus
pattern SourceCollectionStatus_MISCONFIGURED = SourceCollectionStatus' "MISCONFIGURED"

pattern SourceCollectionStatus_NOT_COLLECTING :: SourceCollectionStatus
pattern SourceCollectionStatus_NOT_COLLECTING = SourceCollectionStatus' "NOT_COLLECTING"

{-# COMPLETE
  SourceCollectionStatus_COLLECTING,
  SourceCollectionStatus_MISCONFIGURED,
  SourceCollectionStatus_NOT_COLLECTING,
  SourceCollectionStatus'
  #-}
