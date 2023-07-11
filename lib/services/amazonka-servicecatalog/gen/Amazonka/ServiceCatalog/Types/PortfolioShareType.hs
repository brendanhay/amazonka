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
-- Module      : Amazonka.ServiceCatalog.Types.PortfolioShareType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.PortfolioShareType
  ( PortfolioShareType
      ( ..,
        PortfolioShareType_AWS_ORGANIZATIONS,
        PortfolioShareType_AWS_SERVICECATALOG,
        PortfolioShareType_IMPORTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PortfolioShareType = PortfolioShareType'
  { fromPortfolioShareType ::
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
