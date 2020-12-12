{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Application
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Application
  ( Application
      ( Application',
        MicrosoftOffice2016,
        MicrosoftOffice2019
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Application = Application' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern MicrosoftOffice2016 :: Application
pattern MicrosoftOffice2016 = Application' "Microsoft_Office_2016"

pattern MicrosoftOffice2019 :: Application
pattern MicrosoftOffice2019 = Application' "Microsoft_Office_2019"

{-# COMPLETE
  MicrosoftOffice2016,
  MicrosoftOffice2019,
  Application'
  #-}
