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
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
  ( ServiceUpdateSeverity
      ( ..,
        ServiceUpdateSeverity_Critical,
        ServiceUpdateSeverity_Important,
        ServiceUpdateSeverity_Low,
        ServiceUpdateSeverity_Medium
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ServiceUpdateSeverity = ServiceUpdateSeverity'
  { fromServiceUpdateSeverity ::
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

pattern ServiceUpdateSeverity_Critical :: ServiceUpdateSeverity
pattern ServiceUpdateSeverity_Critical = ServiceUpdateSeverity' "critical"

pattern ServiceUpdateSeverity_Important :: ServiceUpdateSeverity
pattern ServiceUpdateSeverity_Important = ServiceUpdateSeverity' "important"

pattern ServiceUpdateSeverity_Low :: ServiceUpdateSeverity
pattern ServiceUpdateSeverity_Low = ServiceUpdateSeverity' "low"

pattern ServiceUpdateSeverity_Medium :: ServiceUpdateSeverity
pattern ServiceUpdateSeverity_Medium = ServiceUpdateSeverity' "medium"

{-# COMPLETE
  ServiceUpdateSeverity_Critical,
  ServiceUpdateSeverity_Important,
  ServiceUpdateSeverity_Low,
  ServiceUpdateSeverity_Medium,
  ServiceUpdateSeverity'
  #-}
