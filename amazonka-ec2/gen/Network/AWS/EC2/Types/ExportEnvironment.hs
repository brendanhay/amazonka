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
-- Module      : Network.AWS.EC2.Types.ExportEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportEnvironment
  ( ExportEnvironment
      ( ..,
        ExportEnvironment_Citrix,
        ExportEnvironment_Microsoft,
        ExportEnvironment_Vmware
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ExportEnvironment = ExportEnvironment'
  { fromExportEnvironment ::
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

pattern ExportEnvironment_Citrix :: ExportEnvironment
pattern ExportEnvironment_Citrix = ExportEnvironment' "citrix"

pattern ExportEnvironment_Microsoft :: ExportEnvironment
pattern ExportEnvironment_Microsoft = ExportEnvironment' "microsoft"

pattern ExportEnvironment_Vmware :: ExportEnvironment
pattern ExportEnvironment_Vmware = ExportEnvironment' "vmware"

{-# COMPLETE
  ExportEnvironment_Citrix,
  ExportEnvironment_Microsoft,
  ExportEnvironment_Vmware,
  ExportEnvironment'
  #-}
