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
-- Module      : Amazonka.CloudFront.Types.ViewerProtocolPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ViewerProtocolPolicy
  ( ViewerProtocolPolicy
      ( ..,
        ViewerProtocolPolicy_Allow_all,
        ViewerProtocolPolicy_Https_only,
        ViewerProtocolPolicy_Redirect_to_https
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ViewerProtocolPolicy = ViewerProtocolPolicy'
  { fromViewerProtocolPolicy ::
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
