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
-- Module      : Amazonka.QuickSight.Types.FolderFilterAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FolderFilterAttribute
  ( FolderFilterAttribute
      ( ..,
        FolderFilterAttribute_DIRECT_QUICKSIGHT_OWNER,
        FolderFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER,
        FolderFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER,
        FolderFilterAttribute_FOLDER_NAME,
        FolderFilterAttribute_PARENT_FOLDER_ARN,
        FolderFilterAttribute_QUICKSIGHT_OWNER,
        FolderFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FolderFilterAttribute = FolderFilterAttribute'
  { fromFolderFilterAttribute ::
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

pattern FolderFilterAttribute_DIRECT_QUICKSIGHT_OWNER :: FolderFilterAttribute
pattern FolderFilterAttribute_DIRECT_QUICKSIGHT_OWNER = FolderFilterAttribute' "DIRECT_QUICKSIGHT_OWNER"

pattern FolderFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER :: FolderFilterAttribute
pattern FolderFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER = FolderFilterAttribute' "DIRECT_QUICKSIGHT_SOLE_OWNER"

pattern FolderFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER :: FolderFilterAttribute
pattern FolderFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER = FolderFilterAttribute' "DIRECT_QUICKSIGHT_VIEWER_OR_OWNER"

pattern FolderFilterAttribute_FOLDER_NAME :: FolderFilterAttribute
pattern FolderFilterAttribute_FOLDER_NAME = FolderFilterAttribute' "FOLDER_NAME"

pattern FolderFilterAttribute_PARENT_FOLDER_ARN :: FolderFilterAttribute
pattern FolderFilterAttribute_PARENT_FOLDER_ARN = FolderFilterAttribute' "PARENT_FOLDER_ARN"

pattern FolderFilterAttribute_QUICKSIGHT_OWNER :: FolderFilterAttribute
pattern FolderFilterAttribute_QUICKSIGHT_OWNER = FolderFilterAttribute' "QUICKSIGHT_OWNER"

pattern FolderFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER :: FolderFilterAttribute
pattern FolderFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER = FolderFilterAttribute' "QUICKSIGHT_VIEWER_OR_OWNER"

{-# COMPLETE
  FolderFilterAttribute_DIRECT_QUICKSIGHT_OWNER,
  FolderFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER,
  FolderFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER,
  FolderFilterAttribute_FOLDER_NAME,
  FolderFilterAttribute_PARENT_FOLDER_ARN,
  FolderFilterAttribute_QUICKSIGHT_OWNER,
  FolderFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER,
  FolderFilterAttribute'
  #-}
