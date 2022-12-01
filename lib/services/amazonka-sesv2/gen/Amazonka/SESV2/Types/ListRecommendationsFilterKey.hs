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
-- Module      : Amazonka.SESV2.Types.ListRecommendationsFilterKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ListRecommendationsFilterKey
  ( ListRecommendationsFilterKey
      ( ..,
        ListRecommendationsFilterKey_IMPACT,
        ListRecommendationsFilterKey_RESOURCE_ARN,
        ListRecommendationsFilterKey_STATUS,
        ListRecommendationsFilterKey_TYPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The @ListRecommendations@ filter type. This can be one of the following:
--
-- -   @TYPE@ – The recommendation type, with values like @DKIM@, @SPF@ or
--     @DMARC@.
--
-- -   @IMPACT@ – The recommendation impact, with values like @HIGH@ or
--     @LOW@.
--
-- -   @STATUS@ – The recommendation status, with values like @OPEN@ or
--     @FIXED@.
--
-- -   @RESOURCE_ARN@ – The resource affected by the recommendation, with
--     values like
--     @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
newtype ListRecommendationsFilterKey = ListRecommendationsFilterKey'
  { fromListRecommendationsFilterKey ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern ListRecommendationsFilterKey_IMPACT :: ListRecommendationsFilterKey
pattern ListRecommendationsFilterKey_IMPACT = ListRecommendationsFilterKey' "IMPACT"

pattern ListRecommendationsFilterKey_RESOURCE_ARN :: ListRecommendationsFilterKey
pattern ListRecommendationsFilterKey_RESOURCE_ARN = ListRecommendationsFilterKey' "RESOURCE_ARN"

pattern ListRecommendationsFilterKey_STATUS :: ListRecommendationsFilterKey
pattern ListRecommendationsFilterKey_STATUS = ListRecommendationsFilterKey' "STATUS"

pattern ListRecommendationsFilterKey_TYPE :: ListRecommendationsFilterKey
pattern ListRecommendationsFilterKey_TYPE = ListRecommendationsFilterKey' "TYPE"

{-# COMPLETE
  ListRecommendationsFilterKey_IMPACT,
  ListRecommendationsFilterKey_RESOURCE_ARN,
  ListRecommendationsFilterKey_STATUS,
  ListRecommendationsFilterKey_TYPE,
  ListRecommendationsFilterKey'
  #-}
