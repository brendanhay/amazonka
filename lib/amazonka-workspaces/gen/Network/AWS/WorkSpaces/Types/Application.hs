{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Application
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Application
  ( Application
    ( Application'
    , ApplicationMicrosoftOffice2016
    , ApplicationMicrosoftOffice2019
    , fromApplication
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Application = Application'{fromApplication :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern ApplicationMicrosoftOffice2016 :: Application
pattern ApplicationMicrosoftOffice2016 = Application' "Microsoft_Office_2016"

pattern ApplicationMicrosoftOffice2019 :: Application
pattern ApplicationMicrosoftOffice2019 = Application' "Microsoft_Office_2019"

{-# COMPLETE 
  ApplicationMicrosoftOffice2016,

  ApplicationMicrosoftOffice2019,
  Application'
  #-}
