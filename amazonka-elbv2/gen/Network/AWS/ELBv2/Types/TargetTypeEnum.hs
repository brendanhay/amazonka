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
-- Module      : Network.AWS.ELBv2.Types.TargetTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetTypeEnum
  ( TargetTypeEnum
      ( ..,
        TargetTypeEnum_Instance,
        TargetTypeEnum_Ip,
        TargetTypeEnum_Lambda
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TargetTypeEnum = TargetTypeEnum'
  { fromTargetTypeEnum ::
      Prelude.Text
  }
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

pattern TargetTypeEnum_Instance :: TargetTypeEnum
pattern TargetTypeEnum_Instance = TargetTypeEnum' "instance"

pattern TargetTypeEnum_Ip :: TargetTypeEnum
pattern TargetTypeEnum_Ip = TargetTypeEnum' "ip"

pattern TargetTypeEnum_Lambda :: TargetTypeEnum
pattern TargetTypeEnum_Lambda = TargetTypeEnum' "lambda"

{-# COMPLETE
  TargetTypeEnum_Instance,
  TargetTypeEnum_Ip,
  TargetTypeEnum_Lambda,
  TargetTypeEnum'
  #-}
