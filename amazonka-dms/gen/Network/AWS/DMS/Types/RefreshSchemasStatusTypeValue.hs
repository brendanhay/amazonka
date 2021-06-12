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
-- Module      : Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
  ( RefreshSchemasStatusTypeValue
      ( ..,
        RefreshSchemasStatusTypeValue_Failed,
        RefreshSchemasStatusTypeValue_Refreshing,
        RefreshSchemasStatusTypeValue_Successful
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RefreshSchemasStatusTypeValue = RefreshSchemasStatusTypeValue'
  { fromRefreshSchemasStatusTypeValue ::
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

pattern RefreshSchemasStatusTypeValue_Failed :: RefreshSchemasStatusTypeValue
pattern RefreshSchemasStatusTypeValue_Failed = RefreshSchemasStatusTypeValue' "failed"

pattern RefreshSchemasStatusTypeValue_Refreshing :: RefreshSchemasStatusTypeValue
pattern RefreshSchemasStatusTypeValue_Refreshing = RefreshSchemasStatusTypeValue' "refreshing"

pattern RefreshSchemasStatusTypeValue_Successful :: RefreshSchemasStatusTypeValue
pattern RefreshSchemasStatusTypeValue_Successful = RefreshSchemasStatusTypeValue' "successful"

{-# COMPLETE
  RefreshSchemasStatusTypeValue_Failed,
  RefreshSchemasStatusTypeValue_Refreshing,
  RefreshSchemasStatusTypeValue_Successful,
  RefreshSchemasStatusTypeValue'
  #-}
