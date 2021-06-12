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
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagFilterType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.EC2TagFilterType
  ( EC2TagFilterType
      ( ..,
        EC2TagFilterType_KEY_AND_VALUE,
        EC2TagFilterType_KEY_ONLY,
        EC2TagFilterType_VALUE_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EC2TagFilterType = EC2TagFilterType'
  { fromEC2TagFilterType ::
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

pattern EC2TagFilterType_KEY_AND_VALUE :: EC2TagFilterType
pattern EC2TagFilterType_KEY_AND_VALUE = EC2TagFilterType' "KEY_AND_VALUE"

pattern EC2TagFilterType_KEY_ONLY :: EC2TagFilterType
pattern EC2TagFilterType_KEY_ONLY = EC2TagFilterType' "KEY_ONLY"

pattern EC2TagFilterType_VALUE_ONLY :: EC2TagFilterType
pattern EC2TagFilterType_VALUE_ONLY = EC2TagFilterType' "VALUE_ONLY"

{-# COMPLETE
  EC2TagFilterType_KEY_AND_VALUE,
  EC2TagFilterType_KEY_ONLY,
  EC2TagFilterType_VALUE_ONLY,
  EC2TagFilterType'
  #-}
