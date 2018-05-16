{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Network.AWS.ELB.Internal
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Internal
    ( getAccountId
    ) where

import Network.AWS.Prelude

-- | This account identifier is used when attaching a policy to your S3 bucket
-- allowing ELB to upload and write access logs.
--
-- /See:/ <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-access-logs.html#attach-bucket-policy Attach a Policy to Your S3 Bucket>.
getAccountId :: Region -> Text
getAccountId = \case
    NorthVirginia   -> "127311923021"
    Ohio            -> "033677994240"
    NorthCalifornia -> "027434742980"
    Oregon          -> "797873946194"
    Montreal        -> "985666609251"
    Tokyo           -> "582318560864"
    Seoul           -> "600734575887"
    Mumbai          -> "718504428378"
    Singapore       -> "114774131450"
    Sydney          -> "783225319266"
    Ireland         -> "156460612806"
    London          -> "652711504416"
    Frankfurt       -> "054676820928"
    SaoPaulo        -> "507241528517"
    GovCloud        -> "048591011584"
    GovCloudFIPS    -> "048591011584"
    Beijing         -> "638102146993"
