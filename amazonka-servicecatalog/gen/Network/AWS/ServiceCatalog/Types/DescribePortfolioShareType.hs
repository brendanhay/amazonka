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
-- Module      : Network.AWS.ServiceCatalog.Types.DescribePortfolioShareType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.DescribePortfolioShareType
  ( DescribePortfolioShareType
      ( ..,
        DescribePortfolioShareType_ACCOUNT,
        DescribePortfolioShareType_ORGANIZATION,
        DescribePortfolioShareType_ORGANIZATIONAL_UNIT,
        DescribePortfolioShareType_ORGANIZATION_MEMBER_ACCOUNT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DescribePortfolioShareType = DescribePortfolioShareType'
  { fromDescribePortfolioShareType ::
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

pattern DescribePortfolioShareType_ACCOUNT :: DescribePortfolioShareType
pattern DescribePortfolioShareType_ACCOUNT = DescribePortfolioShareType' "ACCOUNT"

pattern DescribePortfolioShareType_ORGANIZATION :: DescribePortfolioShareType
pattern DescribePortfolioShareType_ORGANIZATION = DescribePortfolioShareType' "ORGANIZATION"

pattern DescribePortfolioShareType_ORGANIZATIONAL_UNIT :: DescribePortfolioShareType
pattern DescribePortfolioShareType_ORGANIZATIONAL_UNIT = DescribePortfolioShareType' "ORGANIZATIONAL_UNIT"

pattern DescribePortfolioShareType_ORGANIZATION_MEMBER_ACCOUNT :: DescribePortfolioShareType
pattern DescribePortfolioShareType_ORGANIZATION_MEMBER_ACCOUNT = DescribePortfolioShareType' "ORGANIZATION_MEMBER_ACCOUNT"

{-# COMPLETE
  DescribePortfolioShareType_ACCOUNT,
  DescribePortfolioShareType_ORGANIZATION,
  DescribePortfolioShareType_ORGANIZATIONAL_UNIT,
  DescribePortfolioShareType_ORGANIZATION_MEMBER_ACCOUNT,
  DescribePortfolioShareType'
  #-}
