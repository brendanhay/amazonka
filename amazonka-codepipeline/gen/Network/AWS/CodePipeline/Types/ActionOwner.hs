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
-- Module      : Network.AWS.CodePipeline.Types.ActionOwner
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionOwner
  ( ActionOwner
      ( ..,
        ActionOwner_AWS,
        ActionOwner_Custom,
        ActionOwner_ThirdParty
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ActionOwner = ActionOwner'
  { fromActionOwner ::
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
