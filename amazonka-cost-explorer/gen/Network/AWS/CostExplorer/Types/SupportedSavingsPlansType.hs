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
-- Module      : Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
  ( SupportedSavingsPlansType
      ( ..,
        SupportedSavingsPlansType_COMPUTE_SP,
        SupportedSavingsPlansType_EC2_INSTANCE_SP
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SupportedSavingsPlansType = SupportedSavingsPlansType'
  { fromSupportedSavingsPlansType ::
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

pattern SupportedSavingsPlansType_COMPUTE_SP :: SupportedSavingsPlansType
pattern SupportedSavingsPlansType_COMPUTE_SP = SupportedSavingsPlansType' "COMPUTE_SP"

pattern SupportedSavingsPlansType_EC2_INSTANCE_SP :: SupportedSavingsPlansType
pattern SupportedSavingsPlansType_EC2_INSTANCE_SP = SupportedSavingsPlansType' "EC2_INSTANCE_SP"

{-# COMPLETE
  SupportedSavingsPlansType_COMPUTE_SP,
  SupportedSavingsPlansType_EC2_INSTANCE_SP,
  SupportedSavingsPlansType'
  #-}
