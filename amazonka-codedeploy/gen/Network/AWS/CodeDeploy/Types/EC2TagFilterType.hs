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

import qualified Network.AWS.Prelude as Prelude

newtype EC2TagFilterType = EC2TagFilterType'
  { fromEC2TagFilterType ::
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
