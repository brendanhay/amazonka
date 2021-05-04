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
-- Module      : Network.AWS.Greengrass.Types.SoftwareToUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SoftwareToUpdate
  ( SoftwareToUpdate
      ( ..,
        SoftwareToUpdate_Core,
        SoftwareToUpdate_Ota_agent
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The piece of software on the Greengrass core that will be updated.
newtype SoftwareToUpdate = SoftwareToUpdate'
  { fromSoftwareToUpdate ::
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

pattern SoftwareToUpdate_Core :: SoftwareToUpdate
pattern SoftwareToUpdate_Core = SoftwareToUpdate' "core"

pattern SoftwareToUpdate_Ota_agent :: SoftwareToUpdate
pattern SoftwareToUpdate_Ota_agent = SoftwareToUpdate' "ota_agent"

{-# COMPLETE
  SoftwareToUpdate_Core,
  SoftwareToUpdate_Ota_agent,
  SoftwareToUpdate'
  #-}
