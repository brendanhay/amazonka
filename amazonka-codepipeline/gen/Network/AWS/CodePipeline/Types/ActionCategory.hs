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
-- Module      : Network.AWS.CodePipeline.Types.ActionCategory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionCategory
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

import qualified Network.AWS.Prelude as Prelude

newtype ActionCategory = ActionCategory'
  { fromActionCategory ::
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
