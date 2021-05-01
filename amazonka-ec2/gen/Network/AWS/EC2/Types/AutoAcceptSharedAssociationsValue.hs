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
-- Module      : Network.AWS.EC2.Types.AutoAcceptSharedAssociationsValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AutoAcceptSharedAssociationsValue
  ( AutoAcceptSharedAssociationsValue
      ( ..,
        AutoAcceptSharedAssociationsValue_Disable,
        AutoAcceptSharedAssociationsValue_Enable
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype AutoAcceptSharedAssociationsValue = AutoAcceptSharedAssociationsValue'
  { fromAutoAcceptSharedAssociationsValue ::
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

pattern AutoAcceptSharedAssociationsValue_Disable :: AutoAcceptSharedAssociationsValue
pattern AutoAcceptSharedAssociationsValue_Disable = AutoAcceptSharedAssociationsValue' "disable"

pattern AutoAcceptSharedAssociationsValue_Enable :: AutoAcceptSharedAssociationsValue
pattern AutoAcceptSharedAssociationsValue_Enable = AutoAcceptSharedAssociationsValue' "enable"

{-# COMPLETE
  AutoAcceptSharedAssociationsValue_Disable,
  AutoAcceptSharedAssociationsValue_Enable,
  AutoAcceptSharedAssociationsValue'
  #-}
