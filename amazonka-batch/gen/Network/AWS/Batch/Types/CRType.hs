{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype CRType = CRType' {fromCRType :: Prelude.Text}
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
