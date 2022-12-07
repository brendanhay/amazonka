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
-- Module      : Amazonka.Batch.Types.CRType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.CRType
  ( CRType
      ( ..,
        CRType_EC2,
        CRType_FARGATE,
        CRType_FARGATE_SPOT,
        CRType_SPOT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CRType = CRType' {fromCRType :: Data.Text}
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

pattern CRType_EC2 :: CRType
pattern CRType_EC2 = CRType' "EC2"

pattern CRType_FARGATE :: CRType
pattern CRType_FARGATE = CRType' "FARGATE"

pattern CRType_FARGATE_SPOT :: CRType
pattern CRType_FARGATE_SPOT = CRType' "FARGATE_SPOT"

pattern CRType_SPOT :: CRType
pattern CRType_SPOT = CRType' "SPOT"

{-# COMPLETE
  CRType_EC2,
  CRType_FARGATE,
  CRType_FARGATE_SPOT,
  CRType_SPOT,
  CRType'
  #-}
