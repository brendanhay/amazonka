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
-- Module      : Amazonka.WorkSpaces.Types.DeletableSamlProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.DeletableSamlProperty
  ( DeletableSamlProperty
      ( ..,
        DeletableSamlProperty_SAML_PROPERTIES_RELAY_STATE_PARAMETER_NAME,
        DeletableSamlProperty_SAML_PROPERTIES_USER_ACCESS_URL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeletableSamlProperty = DeletableSamlProperty'
  { fromDeletableSamlProperty ::
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

pattern DeletableSamlProperty_SAML_PROPERTIES_RELAY_STATE_PARAMETER_NAME :: DeletableSamlProperty
pattern DeletableSamlProperty_SAML_PROPERTIES_RELAY_STATE_PARAMETER_NAME = DeletableSamlProperty' "SAML_PROPERTIES_RELAY_STATE_PARAMETER_NAME"

pattern DeletableSamlProperty_SAML_PROPERTIES_USER_ACCESS_URL :: DeletableSamlProperty
pattern DeletableSamlProperty_SAML_PROPERTIES_USER_ACCESS_URL = DeletableSamlProperty' "SAML_PROPERTIES_USER_ACCESS_URL"

{-# COMPLETE
  DeletableSamlProperty_SAML_PROPERTIES_RELAY_STATE_PARAMETER_NAME,
  DeletableSamlProperty_SAML_PROPERTIES_USER_ACCESS_URL,
  DeletableSamlProperty'
  #-}
