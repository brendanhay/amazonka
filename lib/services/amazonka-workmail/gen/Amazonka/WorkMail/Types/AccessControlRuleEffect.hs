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
-- Module      : Amazonka.WorkMail.Types.AccessControlRuleEffect
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.AccessControlRuleEffect
  ( AccessControlRuleEffect
      ( ..,
        AccessControlRuleEffect_ALLOW,
        AccessControlRuleEffect_DENY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccessControlRuleEffect = AccessControlRuleEffect'
  { fromAccessControlRuleEffect ::
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

pattern AccessControlRuleEffect_ALLOW :: AccessControlRuleEffect
pattern AccessControlRuleEffect_ALLOW = AccessControlRuleEffect' "ALLOW"

pattern AccessControlRuleEffect_DENY :: AccessControlRuleEffect
pattern AccessControlRuleEffect_DENY = AccessControlRuleEffect' "DENY"

{-# COMPLETE
  AccessControlRuleEffect_ALLOW,
  AccessControlRuleEffect_DENY,
  AccessControlRuleEffect'
  #-}
