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
-- Module      : Network.AWS.MediaConvert.Types.H265GopBReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265GopBReference
  ( H265GopBReference
      ( ..,
        H265GopBReference_DISABLED,
        H265GopBReference_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | If enable, use reference B frames for GOP structures that have B frames
-- > 1.
newtype H265GopBReference = H265GopBReference'
  { fromH265GopBReference ::
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

pattern H265GopBReference_DISABLED :: H265GopBReference
pattern H265GopBReference_DISABLED = H265GopBReference' "DISABLED"

pattern H265GopBReference_ENABLED :: H265GopBReference
pattern H265GopBReference_ENABLED = H265GopBReference' "ENABLED"

{-# COMPLETE
  H265GopBReference_DISABLED,
  H265GopBReference_ENABLED,
  H265GopBReference'
  #-}
