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
-- Module      : Amazonka.Kendra.Types.SharePointVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SharePointVersion
  ( SharePointVersion
      ( ..,
        SharePointVersion_SHAREPOINT_2013,
        SharePointVersion_SHAREPOINT_2016,
        SharePointVersion_SHAREPOINT_2019,
        SharePointVersion_SHAREPOINT_ONLINE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SharePointVersion = SharePointVersion'
  { fromSharePointVersion ::
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

pattern SharePointVersion_SHAREPOINT_2013 :: SharePointVersion
pattern SharePointVersion_SHAREPOINT_2013 = SharePointVersion' "SHAREPOINT_2013"

pattern SharePointVersion_SHAREPOINT_2016 :: SharePointVersion
pattern SharePointVersion_SHAREPOINT_2016 = SharePointVersion' "SHAREPOINT_2016"

pattern SharePointVersion_SHAREPOINT_2019 :: SharePointVersion
pattern SharePointVersion_SHAREPOINT_2019 = SharePointVersion' "SHAREPOINT_2019"

pattern SharePointVersion_SHAREPOINT_ONLINE :: SharePointVersion
pattern SharePointVersion_SHAREPOINT_ONLINE = SharePointVersion' "SHAREPOINT_ONLINE"

{-# COMPLETE
  SharePointVersion_SHAREPOINT_2013,
  SharePointVersion_SHAREPOINT_2016,
  SharePointVersion_SHAREPOINT_2019,
  SharePointVersion_SHAREPOINT_ONLINE,
  SharePointVersion'
  #-}
