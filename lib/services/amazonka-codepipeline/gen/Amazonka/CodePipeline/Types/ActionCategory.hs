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
-- Module      : Amazonka.CodePipeline.Types.ActionCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionCategory
  ( ActionCategory
      ( ..,
        ActionCategory_Approval,
        ActionCategory_Build,
        ActionCategory_Deploy,
        ActionCategory_Invoke,
        ActionCategory_Source,
        ActionCategory_Test
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionCategory = ActionCategory'
  { fromActionCategory ::
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

pattern ActionCategory_Approval :: ActionCategory
pattern ActionCategory_Approval = ActionCategory' "Approval"

pattern ActionCategory_Build :: ActionCategory
pattern ActionCategory_Build = ActionCategory' "Build"

pattern ActionCategory_Deploy :: ActionCategory
pattern ActionCategory_Deploy = ActionCategory' "Deploy"

pattern ActionCategory_Invoke :: ActionCategory
pattern ActionCategory_Invoke = ActionCategory' "Invoke"

pattern ActionCategory_Source :: ActionCategory
pattern ActionCategory_Source = ActionCategory' "Source"

pattern ActionCategory_Test :: ActionCategory
pattern ActionCategory_Test = ActionCategory' "Test"

{-# COMPLETE
  ActionCategory_Approval,
  ActionCategory_Build,
  ActionCategory_Deploy,
  ActionCategory_Invoke,
  ActionCategory_Source,
  ActionCategory_Test,
  ActionCategory'
  #-}
