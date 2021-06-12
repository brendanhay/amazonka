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
-- Module      : Network.AWS.SageMaker.Types.AppImageConfigSortKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppImageConfigSortKey
  ( AppImageConfigSortKey
      ( ..,
        AppImageConfigSortKey_CreationTime,
        AppImageConfigSortKey_LastModifiedTime,
        AppImageConfigSortKey_Name
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AppImageConfigSortKey = AppImageConfigSortKey'
  { fromAppImageConfigSortKey ::
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

pattern AppImageConfigSortKey_CreationTime :: AppImageConfigSortKey
pattern AppImageConfigSortKey_CreationTime = AppImageConfigSortKey' "CreationTime"

pattern AppImageConfigSortKey_LastModifiedTime :: AppImageConfigSortKey
pattern AppImageConfigSortKey_LastModifiedTime = AppImageConfigSortKey' "LastModifiedTime"

pattern AppImageConfigSortKey_Name :: AppImageConfigSortKey
pattern AppImageConfigSortKey_Name = AppImageConfigSortKey' "Name"

{-# COMPLETE
  AppImageConfigSortKey_CreationTime,
  AppImageConfigSortKey_LastModifiedTime,
  AppImageConfigSortKey_Name,
  AppImageConfigSortKey'
  #-}
