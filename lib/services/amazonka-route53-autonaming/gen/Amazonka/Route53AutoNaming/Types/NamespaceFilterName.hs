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
-- Module      : Amazonka.Route53AutoNaming.Types.NamespaceFilterName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.NamespaceFilterName
  ( NamespaceFilterName
      ( ..,
        NamespaceFilterName_HTTP_NAME,
        NamespaceFilterName_NAME,
        NamespaceFilterName_TYPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NamespaceFilterName = NamespaceFilterName'
  { fromNamespaceFilterName ::
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

pattern NamespaceFilterName_HTTP_NAME :: NamespaceFilterName
pattern NamespaceFilterName_HTTP_NAME = NamespaceFilterName' "HTTP_NAME"

pattern NamespaceFilterName_NAME :: NamespaceFilterName
pattern NamespaceFilterName_NAME = NamespaceFilterName' "NAME"

pattern NamespaceFilterName_TYPE :: NamespaceFilterName
pattern NamespaceFilterName_TYPE = NamespaceFilterName' "TYPE"

{-# COMPLETE
  NamespaceFilterName_HTTP_NAME,
  NamespaceFilterName_NAME,
  NamespaceFilterName_TYPE,
  NamespaceFilterName'
  #-}
