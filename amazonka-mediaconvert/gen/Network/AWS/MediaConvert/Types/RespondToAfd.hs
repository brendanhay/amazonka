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
-- Module      : Network.AWS.MediaConvert.Types.RespondToAfd
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.RespondToAfd
  ( RespondToAfd
      ( ..,
        RespondToAfd_NONE,
        RespondToAfd_PASSTHROUGH,
        RespondToAfd_RESPOND
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the
-- video itself in response to AFD values in the input. * Choose Respond to
-- clip the input video frame according to the AFD value, input display
-- aspect ratio, and output display aspect ratio. * Choose Passthrough to
-- include the input AFD values. Do not choose this when AfdSignaling is
-- set to (NONE). A preferred implementation of this workflow is to set
-- RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to
-- remove all input AFD values from this output.
newtype RespondToAfd = RespondToAfd'
  { fromRespondToAfd ::
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

pattern RespondToAfd_NONE :: RespondToAfd
pattern RespondToAfd_NONE = RespondToAfd' "NONE"

pattern RespondToAfd_PASSTHROUGH :: RespondToAfd
pattern RespondToAfd_PASSTHROUGH = RespondToAfd' "PASSTHROUGH"

pattern RespondToAfd_RESPOND :: RespondToAfd
pattern RespondToAfd_RESPOND = RespondToAfd' "RESPOND"

{-# COMPLETE
  RespondToAfd_NONE,
  RespondToAfd_PASSTHROUGH,
  RespondToAfd_RESPOND,
  RespondToAfd'
  #-}
