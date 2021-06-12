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
-- Module      : Network.AWS.Batch.Types.CRType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.CRType
  ( CRType
      ( ..,
        CRType_EC2,
        CRType_FARGATE,
        CRType_FARGATE_SPOT,
        CRType_SPOT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CRType = CRType' {fromCRType :: Core.Text}
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
