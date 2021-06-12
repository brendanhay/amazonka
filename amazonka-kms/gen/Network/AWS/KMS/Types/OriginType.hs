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
-- Module      : Network.AWS.KMS.Types.OriginType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.OriginType
  ( OriginType
      ( ..,
        OriginType_AWS_CLOUDHSM,
        OriginType_AWS_KMS,
        OriginType_EXTERNAL
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OriginType = OriginType'
  { fromOriginType ::
      Core.Text
  }
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

pattern OriginType_AWS_CLOUDHSM :: OriginType
pattern OriginType_AWS_CLOUDHSM = OriginType' "AWS_CLOUDHSM"

pattern OriginType_AWS_KMS :: OriginType
pattern OriginType_AWS_KMS = OriginType' "AWS_KMS"

pattern OriginType_EXTERNAL :: OriginType
pattern OriginType_EXTERNAL = OriginType' "EXTERNAL"

{-# COMPLETE
  OriginType_AWS_CLOUDHSM,
  OriginType_AWS_KMS,
  OriginType_EXTERNAL,
  OriginType'
  #-}
