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
-- Module      : Network.AWS.SageMaker.Types.HumanTaskUiStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanTaskUiStatus
  ( HumanTaskUiStatus
      ( ..,
        HumanTaskUiStatus_Active,
        HumanTaskUiStatus_Deleting
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype HumanTaskUiStatus = HumanTaskUiStatus'
  { fromHumanTaskUiStatus ::
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

pattern HumanTaskUiStatus_Active :: HumanTaskUiStatus
pattern HumanTaskUiStatus_Active = HumanTaskUiStatus' "Active"

pattern HumanTaskUiStatus_Deleting :: HumanTaskUiStatus
pattern HumanTaskUiStatus_Deleting = HumanTaskUiStatus' "Deleting"

{-# COMPLETE
  HumanTaskUiStatus_Active,
  HumanTaskUiStatus_Deleting,
  HumanTaskUiStatus'
  #-}
