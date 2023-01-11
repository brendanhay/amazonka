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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
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
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
