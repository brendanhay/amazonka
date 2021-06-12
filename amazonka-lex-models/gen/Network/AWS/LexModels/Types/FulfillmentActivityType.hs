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
-- Module      : Network.AWS.LexModels.Types.FulfillmentActivityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.FulfillmentActivityType
  ( FulfillmentActivityType
      ( ..,
        FulfillmentActivityType_CodeHook,
        FulfillmentActivityType_ReturnIntent
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype FulfillmentActivityType = FulfillmentActivityType'
  { fromFulfillmentActivityType ::
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

pattern FulfillmentActivityType_CodeHook :: FulfillmentActivityType
pattern FulfillmentActivityType_CodeHook = FulfillmentActivityType' "CodeHook"

pattern FulfillmentActivityType_ReturnIntent :: FulfillmentActivityType
pattern FulfillmentActivityType_ReturnIntent = FulfillmentActivityType' "ReturnIntent"

{-# COMPLETE
  FulfillmentActivityType_CodeHook,
  FulfillmentActivityType_ReturnIntent,
  FulfillmentActivityType'
  #-}
