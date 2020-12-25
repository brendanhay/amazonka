{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobTemplateListBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobTemplateListBy
  ( JobTemplateListBy
      ( JobTemplateListBy',
        JobTemplateListByName,
        JobTemplateListByCreationDate,
        JobTemplateListBySystem,
        fromJobTemplateListBy
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
newtype JobTemplateListBy = JobTemplateListBy'
  { fromJobTemplateListBy ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern JobTemplateListByName :: JobTemplateListBy
pattern JobTemplateListByName = JobTemplateListBy' "NAME"

pattern JobTemplateListByCreationDate :: JobTemplateListBy
pattern JobTemplateListByCreationDate = JobTemplateListBy' "CREATION_DATE"

pattern JobTemplateListBySystem :: JobTemplateListBy
pattern JobTemplateListBySystem = JobTemplateListBy' "SYSTEM"

{-# COMPLETE
  JobTemplateListByName,
  JobTemplateListByCreationDate,
  JobTemplateListBySystem,
  JobTemplateListBy'
  #-}
