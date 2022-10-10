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
-- Module      : Amazonka.Kendra.Types.AlfrescoEntity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AlfrescoEntity
  ( AlfrescoEntity
      ( ..,
        AlfrescoEntity_Blog,
        AlfrescoEntity_DocumentLibrary,
        AlfrescoEntity_Wiki
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AlfrescoEntity = AlfrescoEntity'
  { fromAlfrescoEntity ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AlfrescoEntity_Blog :: AlfrescoEntity
pattern AlfrescoEntity_Blog = AlfrescoEntity' "blog"

pattern AlfrescoEntity_DocumentLibrary :: AlfrescoEntity
pattern AlfrescoEntity_DocumentLibrary = AlfrescoEntity' "documentLibrary"

pattern AlfrescoEntity_Wiki :: AlfrescoEntity
pattern AlfrescoEntity_Wiki = AlfrescoEntity' "wiki"

{-# COMPLETE
  AlfrescoEntity_Blog,
  AlfrescoEntity_DocumentLibrary,
  AlfrescoEntity_Wiki,
  AlfrescoEntity'
  #-}
