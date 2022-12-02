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
-- Module      : Amazonka.CodePipeline.Types.ActionOwner
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionOwner
  ( ActionOwner
      ( ..,
        ActionOwner_AWS,
        ActionOwner_Custom,
        ActionOwner_ThirdParty
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionOwner = ActionOwner'
  { fromActionOwner ::
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

pattern ActionOwner_AWS :: ActionOwner
pattern ActionOwner_AWS = ActionOwner' "AWS"

pattern ActionOwner_Custom :: ActionOwner
pattern ActionOwner_Custom = ActionOwner' "Custom"

pattern ActionOwner_ThirdParty :: ActionOwner
pattern ActionOwner_ThirdParty = ActionOwner' "ThirdParty"

{-# COMPLETE
  ActionOwner_AWS,
  ActionOwner_Custom,
  ActionOwner_ThirdParty,
  ActionOwner'
  #-}
