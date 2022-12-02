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
-- Module      : Amazonka.SageMaker.Types.AppSecurityGroupManagement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AppSecurityGroupManagement
  ( AppSecurityGroupManagement
      ( ..,
        AppSecurityGroupManagement_Customer,
        AppSecurityGroupManagement_Service
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AppSecurityGroupManagement = AppSecurityGroupManagement'
  { fromAppSecurityGroupManagement ::
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

pattern AppSecurityGroupManagement_Customer :: AppSecurityGroupManagement
pattern AppSecurityGroupManagement_Customer = AppSecurityGroupManagement' "Customer"

pattern AppSecurityGroupManagement_Service :: AppSecurityGroupManagement
pattern AppSecurityGroupManagement_Service = AppSecurityGroupManagement' "Service"

{-# COMPLETE
  AppSecurityGroupManagement_Customer,
  AppSecurityGroupManagement_Service,
  AppSecurityGroupManagement'
  #-}
