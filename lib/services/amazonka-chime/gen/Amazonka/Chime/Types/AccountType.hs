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
-- Module      : Amazonka.Chime.Types.AccountType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.AccountType
  ( AccountType
      ( ..,
        AccountType_EnterpriseDirectory,
        AccountType_EnterpriseLWA,
        AccountType_EnterpriseOIDC,
        AccountType_Team
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccountType = AccountType'
  { fromAccountType ::
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

pattern AccountType_EnterpriseDirectory :: AccountType
pattern AccountType_EnterpriseDirectory = AccountType' "EnterpriseDirectory"

pattern AccountType_EnterpriseLWA :: AccountType
pattern AccountType_EnterpriseLWA = AccountType' "EnterpriseLWA"

pattern AccountType_EnterpriseOIDC :: AccountType
pattern AccountType_EnterpriseOIDC = AccountType' "EnterpriseOIDC"

pattern AccountType_Team :: AccountType
pattern AccountType_Team = AccountType' "Team"

{-# COMPLETE
  AccountType_EnterpriseDirectory,
  AccountType_EnterpriseLWA,
  AccountType_EnterpriseOIDC,
  AccountType_Team,
  AccountType'
  #-}
