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
-- Module      : Network.AWS.Inspector.Types.ScopeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ScopeType
  ( ScopeType
      ( ..,
        ScopeType_INSTANCE_ID,
        ScopeType_RULES_PACKAGE_ARN
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ScopeType = ScopeType'
  { fromScopeType ::
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

pattern ScopeType_INSTANCE_ID :: ScopeType
pattern ScopeType_INSTANCE_ID = ScopeType' "INSTANCE_ID"

pattern ScopeType_RULES_PACKAGE_ARN :: ScopeType
pattern ScopeType_RULES_PACKAGE_ARN = ScopeType' "RULES_PACKAGE_ARN"

{-# COMPLETE
  ScopeType_INSTANCE_ID,
  ScopeType_RULES_PACKAGE_ARN,
  ScopeType'
  #-}
