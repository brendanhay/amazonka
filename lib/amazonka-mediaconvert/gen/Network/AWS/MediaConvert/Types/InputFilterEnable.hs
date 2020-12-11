-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputFilterEnable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputFilterEnable
  ( InputFilterEnable
      ( InputFilterEnable',
        IFEAuto,
        IFEDisable,
        IFEForce
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
newtype InputFilterEnable = InputFilterEnable' Lude.Text
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

pattern IFEAuto :: InputFilterEnable
pattern IFEAuto = InputFilterEnable' "AUTO"

pattern IFEDisable :: InputFilterEnable
pattern IFEDisable = InputFilterEnable' "DISABLE"

pattern IFEForce :: InputFilterEnable
pattern IFEForce = InputFilterEnable' "FORCE"

{-# COMPLETE
  IFEAuto,
  IFEDisable,
  IFEForce,
  InputFilterEnable'
  #-}
