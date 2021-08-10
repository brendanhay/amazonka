{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Network.AWS.ELB.Internal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Internal
    ( getAccountId
    ) where

import Network.AWS.Core

-- | This account identifier is used when attaching a policy to your S3 bucket
-- allowing ELB to upload and write access logs.
--
-- /See:/ <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-access-logs.html#attach-bucket-policy Attach a Policy to Your S3 Bucket>.
getAccountId :: Region -> Maybe Text
getAccountId = \case
    NorthVirginia   -> Just "127311923021"
    Ohio            -> Just "033677994240"
    NorthCalifornia -> Just "027434742980"
    Oregon          -> Just "797873946194"
    CapeTown        -> Just "098369216593"
    Montreal        -> Just "985666609251"
    Frankfurt       -> Just "054676820928"
    Ireland         -> Just "156460612806"
    London          -> Just "652711504416"
    Milan           -> Just "635631232127"
    Paris           -> Just "009996457667"
    Stockholm       -> Just "897822967062"
    HongKong        -> Just "754344448648"
    Tokyo           -> Just "582318560864"
    Seoul           -> Just "600734575887"
    Osaka           -> Just "383597477331"
    Singapore       -> Just "114774131450"
    Sydney          -> Just "783225319266"
    Mumbai          -> Just "718504428378"
    Bahrain         -> Just "076674570225"
    SaoPaulo        -> Just "507241528517"
    GovCloudWest    -> Just "048591011584"
    GovCloudEast    -> Just "190560391635"
    Beijing         -> Just "638102146993"
    Ningxia         -> Just "037604701340"
    _other          -> Nothing
