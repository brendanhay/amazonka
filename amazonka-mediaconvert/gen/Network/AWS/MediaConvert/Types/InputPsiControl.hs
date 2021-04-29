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
-- Module      : Network.AWS.MediaConvert.Types.InputPsiControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputPsiControl
  ( InputPsiControl
      ( ..,
        InputPsiControl_IGNORE_PSI,
        InputPsiControl_USE_PSI
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify
-- which data the demux process to scans. * Ignore PSI - Scan all PIDs for
-- audio and video. * Use PSI - Scan only PSI data.
newtype InputPsiControl = InputPsiControl'
  { fromInputPsiControl ::
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

pattern InputPsiControl_IGNORE_PSI :: InputPsiControl
pattern InputPsiControl_IGNORE_PSI = InputPsiControl' "IGNORE_PSI"

pattern InputPsiControl_USE_PSI :: InputPsiControl
pattern InputPsiControl_USE_PSI = InputPsiControl' "USE_PSI"

{-# COMPLETE
  InputPsiControl_IGNORE_PSI,
  InputPsiControl_USE_PSI,
  InputPsiControl'
  #-}
