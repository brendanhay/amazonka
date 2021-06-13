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
-- Module      : Network.AWS.ServiceCatalog.Types.PortfolioShareType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.PortfolioShareType
  ( PortfolioShareType
      ( ..,
        PortfolioShareType_AWS_ORGANIZATIONS,
        PortfolioShareType_AWS_SERVICECATALOG,
        PortfolioShareType_IMPORTED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PortfolioShareType = PortfolioShareType'
  { fromPortfolioShareType ::
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

pattern PortfolioShareType_AWS_ORGANIZATIONS :: PortfolioShareType
pattern PortfolioShareType_AWS_ORGANIZATIONS = PortfolioShareType' "AWS_ORGANIZATIONS"

pattern PortfolioShareType_AWS_SERVICECATALOG :: PortfolioShareType
pattern PortfolioShareType_AWS_SERVICECATALOG = PortfolioShareType' "AWS_SERVICECATALOG"

pattern PortfolioShareType_IMPORTED :: PortfolioShareType
pattern PortfolioShareType_IMPORTED = PortfolioShareType' "IMPORTED"

{-# COMPLETE
  PortfolioShareType_AWS_ORGANIZATIONS,
  PortfolioShareType_AWS_SERVICECATALOG,
  PortfolioShareType_IMPORTED,
  PortfolioShareType'
  #-}
