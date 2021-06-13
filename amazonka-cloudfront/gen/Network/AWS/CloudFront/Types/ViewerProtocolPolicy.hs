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
-- Module      : Network.AWS.CloudFront.Types.ViewerProtocolPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ViewerProtocolPolicy
  ( ViewerProtocolPolicy
      ( ..,
        ViewerProtocolPolicy_Allow_all,
        ViewerProtocolPolicy_Https_only,
        ViewerProtocolPolicy_Redirect_to_https
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ViewerProtocolPolicy = ViewerProtocolPolicy'
  { fromViewerProtocolPolicy ::
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

pattern ViewerProtocolPolicy_Allow_all :: ViewerProtocolPolicy
pattern ViewerProtocolPolicy_Allow_all = ViewerProtocolPolicy' "allow-all"

pattern ViewerProtocolPolicy_Https_only :: ViewerProtocolPolicy
pattern ViewerProtocolPolicy_Https_only = ViewerProtocolPolicy' "https-only"

pattern ViewerProtocolPolicy_Redirect_to_https :: ViewerProtocolPolicy
pattern ViewerProtocolPolicy_Redirect_to_https = ViewerProtocolPolicy' "redirect-to-https"

{-# COMPLETE
  ViewerProtocolPolicy_Allow_all,
  ViewerProtocolPolicy_Https_only,
  ViewerProtocolPolicy_Redirect_to_https,
  ViewerProtocolPolicy'
  #-}
